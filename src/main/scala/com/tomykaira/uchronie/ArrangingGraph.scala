package com.tomykaira.uchronie

import org.eclipse.jgit.revwalk.RevCommit
import org.eclipse.jgit.lib.ObjectId

class ArrangingGraph(val repository: GitRepository, val start: ObjectId, val commits: List[RevCommit]) {
  val last = commits.lastOption

  def apply(nth: Int): Option[RevCommit] = {
    if (commits.indices.contains(nth))
      Some(commits(nth))
    else
      None
  }

  def updateComment(target: RevCommit, message: String): Either[String, ArrangingGraph] = {
    val temporaryBranchName = "temp" + hashCode()
    for { // FIXME
      index <- (commits.indexOf(target) match {
        case -1 => Left("Target " + target.getName + " not included in current list")
        case i => Right(i)
      }).right
      orphans <- Right(commits.take(index)).right
      _ <- repository.checkoutAs(target, temporaryBranchName).right
      newHead <- Right(repository.amendMessage(message)).right
      last <- orphans.foldRight[Either[String, RevCommit]](Right(newHead))(
        (c, prev) => prev.right.flatMap(_ => repository.cherryPick(c))).right
    } yield repository.listCommits(start, last)
  }

  def selectRange(values: Seq[Int]) =
    new GraphRange(this, List())

  def contains(range: GraphRange): Boolean =
    true

  def reorder(range: GraphRange, insertTo: Int): Either[String, ArrangingGraph] = {
    Left("Not Implemented")
  }
}

class GraphRange(val graph: ArrangingGraph, val commits: List[RevCommit])
