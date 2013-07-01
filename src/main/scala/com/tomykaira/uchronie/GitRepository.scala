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

  def resolve(sha1: String): Option[ObjectId] = {
    if (sha1.isEmpty)
      return None
    val candidates = repository.getObjectDatabase.newReader.resolve(AbbreviatedObjectId.fromString(sha1))
    if (candidates.size() == 1)
      Some(candidates.asScala.head)
    else
      None
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
    oldParser.reset(repository.newObjectReader(), commit.getParent(0).getTree)
    val newParser = new CanonicalTreeParser()
    newParser.reset(repository.newObjectReader(), commit.getTree)
    git.diff().setOldTree(oldParser).setNewTree(newParser).call().asScala.toList
  }

  def formatDiff(entry: DiffEntry): String = {
    diffFormatter.format(entry)
    val result = diffStream.toString("UTF-8")
    diffStream.reset()
    result
  }

}
