package com.tomykaira.uchronie

import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import java.io.{ByteArrayOutputStream, File}
import org.eclipse.jgit.revwalk.{RevCommit, RevWalk}
import org.eclipse.jgit.lib.{AbbreviatedObjectId, AnyObjectId, ObjectId}
import scala.collection.JavaConverters.{asScalaIteratorConverter,collectionAsScalaIterableConverter}
import org.eclipse.jgit.api._
import scala.Some
import org.eclipse.jgit.treewalk.CanonicalTreeParser
import org.eclipse.jgit.diff.{DiffFormatter, DiffEntry}

class GitRepository(rootPath: File) {
  val repository = new FileRepositoryBuilder().
    setGitDir(rootPath).
    readEnvironment().
    findGitDir().
    build()

  lazy val diffStream = new ByteArrayOutputStream
  lazy val diffFormatter = new DiffFormatter(diffStream)
  diffFormatter.setRepository(repository)

  def listCommits(start: ObjectId, end: ObjectId): ArrangingGraph = {
    val walk = new RevWalk(repository)
    walk.markStart(walk.parseCommit(end))
    walk.markUninteresting(walk.parseCommit(start))
    new ArrangingGraph(this, start, walk.iterator().asScala.toList)
  }

  def abbreviate(objectId: AnyObjectId): AbbreviatedObjectId =
    repository.getObjectDatabase.newReader.abbreviate(objectId)

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

  def git: Git = new Git(repository)

  def checkoutAs(commit: RevCommit, newBranchName: String): Either[String, RevCommit] = {
    val command = git.checkout.setStartPoint(commit).setName(newBranchName).setCreateBranch(true)
    command.call()
    if (command.getResult.getStatus == CheckoutResult.Status.OK)
      Right(commit)
    else
      Left("checkout failed")
  }

  def amendMessage(message: String): RevCommit =
    git.commit.setAmend(true).setMessage(message).call()

  def cherryPick(commit: RevCommit): Either[String, RevCommit] with Product with Serializable = {
    val result = git.cherryPick().include(commit.getId).call()
    if (result.getStatus != CherryPickResult.CherryPickStatus.OK)
      Left("Cherry-pick failed at " + commit.getName)
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

  def isClean: Boolean =
    git.status.call.isClean

  def formatDiff(entry: DiffEntry): String = {
    diffFormatter.format(entry)
    val result = diffStream.toString("UTF-8")
    diffStream.reset()
    result
  }
}
