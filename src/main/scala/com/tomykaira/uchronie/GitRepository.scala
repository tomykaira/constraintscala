package com.tomykaira.uchronie

import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import java.io.File
import org.eclipse.jgit.revwalk.{RevCommit, RevWalk}
import org.eclipse.jgit.lib.{AbbreviatedObjectId, AnyObjectId, ObjectId}
import scala.collection.JavaConverters.asScalaIteratorConverter

class GitRepository(rootPath: File) {
  val repository = new FileRepositoryBuilder().
    setGitDir(rootPath).
    readEnvironment().
    findGitDir().
    build()

  def listCommits(start: ObjectId, end: ObjectId): Seq[RevCommit] = {
    val walk = new RevWalk(repository)
    walk.markStart(walk.parseCommit(end))
    walk.markUninteresting(walk.parseCommit(start))
    walk.iterator().asScala.toList
  }

  def abbreviate(objectId: AnyObjectId): AbbreviatedObjectId =
    repository.getObjectDatabase.newReader.abbreviate(objectId)
}
