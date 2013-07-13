package com.tomykaira.uchronie

import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import java.io.{ByteArrayOutputStream, File}
import org.eclipse.jgit.revwalk.{RevCommit, RevWalk}
import org.eclipse.jgit.lib.{Constants, AbbreviatedObjectId, AnyObjectId, ObjectId}
import scala.collection.JavaConverters.{asScalaIteratorConverter,collectionAsScalaIterableConverter}
import org.eclipse.jgit.api._
import scala.Some
import org.eclipse.jgit.treewalk.CanonicalTreeParser
import org.eclipse.jgit.diff.{DiffFormatter, DiffEntry}

class CherryPickFailure(repository: GitRepository, val original: RevCommit)
  extends RuntimeException(s"Cherry pick failed at ${original.getName}") {

  val message = s"""Cherry pick failed at ${original.getId.abbreviate(7).name}: ${original.getShortMessage}
Conflicts:
  ${repository.status.getConflicting}
"""
  override def getMessage: String = message
}

class NoHeadException extends RuntimeException("no HEAD revision")

class GitRepository(rootPath: File) {
  val repository = new FileRepositoryBuilder().
    setGitDir(rootPath).
    readEnvironment().
    findGitDir().
    build()

  // setup diff formatter
  private[this] lazy val diffStream = new ByteArrayOutputStream
  private[this] lazy val diffFormatter = new DiffFormatter(diffStream)
  diffFormatter.setRepository(repository)

  // setup work branch
  private[this] val originalBranch = repository.getFullBranch
  private[this] lazy val workBranch = "uchronie" + System.currentTimeMillis()
  private[this] val command = git.checkout.setName(workBranch).setCreateBranch(true)
  command.call()
  if (command.getResult.getStatus != CheckoutResult.Status.OK)
    throw new RuntimeException("Failed to initialize work branch")

  def listCommits(start: ObjectId, end: ObjectId): List[RevCommit] = {
    val walk = new RevWalk(repository)
    walk.markStart(walk.parseCommit(end))
    walk.markUninteresting(walk.parseCommit(start))
    walk.iterator().asScala.toList
  }

  def resolve(ref: String): Option[ObjectId] = {
    try {
      val result = repository.resolve(ref)
      if (result == null) None else Some(result)
    } catch {
      case e: Exception => None
    }
  }

  def toCommit(id: ObjectId): RevCommit = {
    new RevWalk(repository).parseCommit(id)
  }

  def head: RevCommit =
    resolve(Constants.HEAD) match {
      case None => throw new NoHeadException
      case Some(rev) => toCommit(rev)
    }

  def git: Git = new Git(repository)

  def amendMessage(message: String): RevCommit =
    git.commit.setAmend(true).setMessage(message).call()

  def cherryPick(commit: RevCommit): Either[CherryPickFailure, RevCommit] = {
    val result = git.cherryPick().include(commit.getId).call()
    if (result.getStatus != CherryPickResult.CherryPickStatus.OK)
      Left(new CherryPickFailure(this, commit))
    else
      Right(result.getNewHead)
  }

  def diff(commit: RevCommit): List[DiffEntry] = {
    val oldParser = new CanonicalTreeParser()
    val parent = toCommit(commit.getParent(0).getId)
    oldParser.reset(repository.newObjectReader(), parent.getTree)
    val newParser = new CanonicalTreeParser()
    newParser.reset(repository.newObjectReader(), commit.getTree)
    git.diff().setOldTree(oldParser).setNewTree(newParser).call().asScala.toList
  }

  def resetHard(commit: ObjectId) {
    reset(commit, ResetCommand.ResetType.HARD)
  }

  def resetSoft(commit: ObjectId) {
    reset(commit, ResetCommand.ResetType.SOFT)
  }

  private def reset(commit: ObjectId, resetType: ResetCommand.ResetType) {
    git.reset().setRef(commit.getName).setMode(resetType).call()
  }

  def commit(message: String): RevCommit = {
    git.commit().setMessage(message).call()
  }

  def status: Status = git.status.call

  def isClean: Boolean = {
    val result = status
    result.getAdded.isEmpty && result.getChanged.isEmpty && result.getRemoved.isEmpty && result.getMissing.isEmpty &&
      result.getModified.isEmpty && result.getConflicting.isEmpty
  }

  def formatDiff(entry: DiffEntry): String = {
    diffFormatter.format(entry)
    val result = diffStream.toString("UTF-8")
    diffStream.reset()
    result
  }

  def resetToOriginalBranch(last: ObjectId) {
    resetHard(last)
    git.checkout().setName(originalBranch).call()
    resetHard(last)
    git.branchDelete().setBranchNames(workBranch).setForce(true).call()
  }
}
