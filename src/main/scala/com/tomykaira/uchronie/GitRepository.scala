package com.tomykaira.uchronie

import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import java.io.File
import org.eclipse.jgit.revwalk.{RevCommit, RevWalk}
import org.eclipse.jgit.lib.{AbbreviatedObjectId, AnyObjectId, ObjectId}
import scala.collection.JavaConverters.asScalaIteratorConverter
import scala.collection.JavaConverters.collectionAsScalaIterableConverter

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

  def resolve(sha1: String): Option[ObjectId] = {
    if (sha1.isEmpty)
      return None
    val candidates = repository.getObjectDatabase.newReader.resolve(AbbreviatedObjectId.fromString(sha1))
    if (candidates.size() == 1)
      Some(candidates.asScala.head)
    else
      None
  }
}
