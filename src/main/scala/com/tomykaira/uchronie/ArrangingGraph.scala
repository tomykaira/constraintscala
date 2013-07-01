package com.tomykaira.uchronie

import org.eclipse.jgit.revwalk.RevCommit
import org.eclipse.jgit.lib.ObjectId
import scala.annotation.tailrec

class ArrangingGraph(val repository: GitRepository, val start: ObjectId, val commits: List[RevCommit]) {
  private lazy val startCommit: RevCommit = repository.toCommit(start)
  private lazy val last = commits.headOption.getOrElse(startCommit)
  private lazy val myBranchName = "temp" + hashCode + System.currentTimeMillis()

  repository.checkoutAs(last, myBranchName)

  def apply(nth: Int): Option[RevCommit] = {
    if (commits.indices.contains(nth))
      Some(commits(nth))
    else
      None
  }

  def updateComment(target: RevCommit, message: String): Either[String, ArrangingGraph] = {
    val result = for { // FIXME
      index <- (commits.indexOf(target) match {
        case -1 => Left("Target " + target.getName + " not included in current list")
        case i => Right(i)
      }).right
      orphans <- Right(commits.take(index)).right
      _ <- Right(repository.resetHard(target)).right
      newHead <- Right(repository.amendMessage(message)).right
      newLast <- applyCommits(newHead, orphans.reverse).right
    } yield newLast
    finishUpdate(result)
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

    val (common, todo) = skipCommonRoot(newOrder.reverse, commits.reverse, startCommit)
    repository.resetHard(common)
    finishUpdate(applyCommits(common, todo))
  }

  def squash(range: GraphRange): Either[String, ArrangingGraph] = {
    Right(this)
  }

  // commits should be ordered from old to new
  private def applyCommits(first: RevCommit, commits: List[RevCommit]): Either[String, RevCommit] = {
    commits.foldLeft[Either[String, RevCommit]](Right(first))(
      (prev, c) => prev.right.flatMap(_ => repository.cherryPick(c)))
  }

  private def finishUpdate(newLast: Either[String, RevCommit]): Either[String, ArrangingGraph] = {
    newLast match {
      case Left(err) =>
        repository.resetHard(last)
        Left(err)
      case Right(c) =>
        Right(repository.listCommits(start, c))
    }
  }

  @tailrec
  private[this] def skipCommonRoot[A](xs: List[A], ys: List[A], common: A): (A, List[A]) = (xs, ys) match {
    case (Nil, _) => (common, Nil)
    case (x::xss, Nil) => (common, x::xss)
    case (x::xss, y::yss) => if (x == y) skipCommonRoot(xss, yss, x) else (common, x::xss)
  }
}

class GraphRange(val graph: ArrangingGraph, val commits: List[RevCommit]) {
  def squash(): Either[String, ArrangingGraph] = {
    graph.squash(this)
  }

  def isEmpty: Boolean = commits.isEmpty
}
