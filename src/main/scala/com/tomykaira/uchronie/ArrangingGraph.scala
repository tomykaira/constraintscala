package com.tomykaira.uchronie

import org.eclipse.jgit.lib.{Constants, ObjectId}
import scala.annotation.tailrec
import com.tomykaira.uchronie.git.{Operation, CommitThread, ThreadTransition, Commit}

class ArrangingGraph(val repository: GitRepository, val start: ObjectId, val last: ObjectId) {
  type OperationResult = Either[String, CommitThread]

  private lazy val startCommit: Commit = repository.toCommit(start)
  lazy val commits: List[Commit.Raw] = repository.listCommits(start, last).map(Commit.Raw)
  val transition: ThreadTransition = new ThreadTransition(CommitThread.fromCommits(commits))

  rollback()

  def apply(nth: Int): Option[Commit] = {
    if (commits.indices.contains(nth))
      Some(commits(nth))
    else
      None
  }

  def currentThread: CommitThread = {
    transition.current
  }

  def rollback() {
    repository.resetHard(last)
  }

  def updateComment(target: Commit, message: String): OperationResult = {
    transition.transit(Operation.RenameOp(target, message)).left.map(_.toString)
  }

  def selectRange(rows: Seq[Int]) = {
    val selected = rows.map(i => this.commits(i)).toList
    new GraphRange(this, selected)
  }

  def contains(range: GraphRange): Boolean = range.graph == this

  def reorder(range: GraphRange, insertTo: Int): OperationResult = {
    if (range.isEmpty) return Right(currentThread)

    transition.transit(Operation.MoveOp(range.commits, insertTo)).left.map(_.toString)
  }

  def squash(range: GraphRange, newMessage: Option[String]): OperationResult = {
    if (range.commits.size <= 1) return Right(currentThread)

    if (range.isEmpty) return Right(currentThread)

    transition.transit(Operation.SquashOp(range.commits, newMessage)).left.map(_.toString)
  }

  def delete(range: GraphRange): OperationResult = {
    range.commits.foldLeft[Either[CommitThread.Error, CommitThread]](Right(currentThread)) { (prev, c) =>
      prev.right.flatMap(_ => transition.transit(Operation.DeleteOp(c)))
    }.left.map(_.toString)
  }

  def startEdit(commit: Commit): GraphRange = {
    val orphans = commits.takeWhile(_ != commit)
    repository.resetHard(commit)
    repository.resetSoft(parent(commit))
    new GraphRange(this, orphans)
  }

  def applyInteractively(range: GraphRange): Either[GraphRange, ArrangingGraph] = {
    if (!repository.isClean)
      return Left(range)
    @tailrec
    def loop(commits: List[Commit]): Either[GraphRange, ArrangingGraph] = commits match {
      case Nil => Right(new ArrangingGraph(repository, start, repository.resolve(Constants.HEAD).get))
      case x :: xs =>
        repository.cherryPick(x) match {
          case Left(err) => Left(new GraphRange(this, xs.reverse))
          case Right(_) => loop(xs)
        }
    }
    loop(range.commits.reverse)
  }

  // parent in the target graph
  private def parent(commit: Commit): Commit = {
    val index = commits.indexOf(commit)
    if(index != -1 && commits.indices.contains(index + 1))
      commits(index+1)
    else
      startCommit
  }

  @tailrec
  private[this] def skipCommonRoot[A](xs: List[A], ys: List[A], common: A): (A, List[A]) = (xs, ys) match {
    case (Nil, _) => (common, Nil)
    case (x::xss, Nil) => (common, x::xss)
    case (x::xss, y::yss) => if (x == y) skipCommonRoot(xss, yss, x) else (common, x::xss)
  }
}

class GraphRange(val graph: ArrangingGraph, val commits: List[Commit]) {
  def squash(newMessage: Option[String]): graph.OperationResult = {
    graph squash(this, newMessage)
  }

  def delete(): graph.OperationResult = {
    graph.delete(this)
  }

  def applyInteractively: Either[GraphRange, ArrangingGraph] =
    graph.applyInteractively(this)

  def isEmpty: Boolean = commits.isEmpty

  def first: Option[Commit] = {
    commits.headOption
  }

  def squashMessage: String= {
    commits.reverse.map(_.getFullMessage.stripLineEnd).mkString("\n\n")
  }
}
