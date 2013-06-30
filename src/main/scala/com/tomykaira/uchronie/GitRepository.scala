package com.tomykaira.uchronie

import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import java.io.File
import org.eclipse.jgit.revwalk.{RevCommit, RevWalk}
import org.eclipse.jgit.lib.{Constants, AbbreviatedObjectId, AnyObjectId, ObjectId}
import scala.collection.JavaConverters.asScalaIteratorConverter
import scala.collection.JavaConverters.collectionAsScalaIterableConverter
import org.eclipse.jgit.api._
import scala.Some

class GitRepository(rootPath: File) {
  val repository = new FileRepositoryBuilder().
    setGitDir(rootPath).
    readEnvironment().
    findGitDir().
    build()

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

  def last: ObjectId =
    repository.resolve(Constants.HEAD)

  def updateComment(commit: RevCommit, message: String): Either[String, RevCommit] = {
    val temporaryBranchName = "temp" + System.nanoTime()
    val orphans = listCommits(commit, last)
    val command = git.checkout.setStartPoint(commit).setName(temporaryBranchName).setCreateBranch(true)
    command.call()
    if (command.getResult.getStatus != CheckoutResult.Status.OK)
      return Left("checkout failed")

    git.commit.setAmend(true).setMessage(message).call()

    val pickCommand = git.cherryPick
    orphans.commits.foreach(c => pickCommand.include(c.getId))
    val result = pickCommand.call()
    if (result.getStatus != CherryPickResult.CherryPickStatus.OK)
      return Left("Cherry-pick failed")
    Right(result.getNewHead)
  }

}
