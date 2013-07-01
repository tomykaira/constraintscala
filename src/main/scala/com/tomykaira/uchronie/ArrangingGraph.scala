package com.tomykaira.uchronie

import org.eclipse.jgit.revwalk.RevCommit
import org.eclipse.jgit.lib.ObjectId
import scala.annotation.tailrec

class ArrangingGraph(val repository: GitRepository, val start: ObjectId, val commits: List[RevCommit]) {
  val last = commits.lastOption

  def apply(nth: Int): Option[RevCommit] = {
    if (commits.indices.contains(nth))
      Some(commits(nth))
    else
      None
  }

  def updateComment(target: RevCommit, message: String): Either[String, ArrangingGraph] = {
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

  def selectRange(rows: Seq[Int]) = {
    val selected = rows.map(i => this.commits(i)).toList
    new GraphRange(this, selected)
  }

  def contains(range: GraphRange): Boolean = range.graph == this

  def reorder(range: GraphRange, insertTo: Int): Either[String, ArrangingGraph] = {
    if (range.isEmpty) return Right(this)

    val newOrder = commits.take(insertTo).filterNot(range.commits contains _) ++
      range.commits ++
      commits.drop(insertTo).filterNot(range.commits contains _)

    val (common, todo) = skipCommonRoot(newOrder.reverse, commits.reverse, repository.toCommit(start))
    repository.checkoutAs(common, temporaryBranchName)
    val result = todo.foldLeft[Either[String, RevCommit]](Right(common))(
      (prev, c) => prev.right.flatMap(_ => repository.cherryPick(c)))
    result.right.map(newLast => repository.listCommits(start, newLast))
  }

  private[this] def temporaryBranchName: String =
    "temp" + hashCode()

  @tailrec
  private[this] def skipCommonRoot[A](xs: List[A], ys: List[A], common: A): (A, List[A]) = (xs, ys) match {
    case (Nil, _) => (common, Nil)
    case (x::xss, Nil) => (common, x::xss)
    case (x::xss, y::yss) => if (x == y) skipCommonRoot(xss, yss, x) else (common, x::xss)
  }
}

class GraphRange(val graph: ArrangingGraph, val commits: List[RevCommit]) {
  def isEmpty: Boolean = commits.isEmpty
}
