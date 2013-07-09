package com.tomykaira.uchronie

import org.eclipse.jgit.lib.{Constants, ObjectId}
import scala.annotation.tailrec
import com.tomykaira.uchronie.git.{Operation, CommitThread, ThreadTransition, Commit}
import com.tomykaira.uchronie.git.Commit.Raw
import scalaz.NonEmptyList

class ArrangingGraph(val repository: GitRepository, val start: ObjectId, val last: ObjectId) {
  type OperationResult = Either[String, CommitThread]

  private lazy val startCommit: Commit = repository.toCommit(start)
  lazy val commits: List[Commit.Raw] = repository.listCommits(start, last).map(Commit.Raw)
  val transition: ThreadTransition = new ThreadTransition(CommitThread.fromCommits(commits))

  rollback()

  def apply(nth: Int): Option[Commit] = {
    // TODO: do not peep thread.commits
    if (currentThread.commits.indices.contains(nth))
      Some(currentThread.commits(nth))
    else
      None
  }

  def currentThread: CommitThread = {
    transition.current
  }

  def transit(op: Operation): OperationResult = {
    transition.transit(op).left.map(_.toString)
  }

  def rollback() {
    repository.resetHard(last)
  }

  def selectRange(rows: NonEmptyList[Int]): GraphRange =
    new GraphRange(this, rowsToCommits(rows))

  def squashMessage(rows: NonEmptyList[Int]): String =
    rowsToCommits(rows).list.reverse.map(_.message.stripLineEnd).mkString("\n\n")

  def rowsToCommits(rows: NonEmptyList[Int]): NonEmptyList[Commit] =
    rows.map(i => currentThread.commits(i))

  def contains(range: GraphRange): Boolean = range.graph == this

  // TODO: accept only Raw
  def startEdit(commit: Commit): Option[GraphRange] = {
    val orphans = commits.takeWhile(_ != commit)
    repository.resetHard(commit.asInstanceOf[Commit.Raw].raw)
    repository.resetSoft(parent(commit).asInstanceOf[Commit.Raw].raw)
    orphans match {
      case Nil => None
      case h :: t => Some(new GraphRange(this, NonEmptyList.nel(h, t)))
    }
  }

  def applyInteractively(range: GraphRange): Either[GraphRange, ArrangingGraph] = {
    if (!repository.isClean)
      return Left(range)
    @tailrec
    def loop(commits: List[Commit]): Either[GraphRange, ArrangingGraph] = commits match {
      case Nil => Right(new ArrangingGraph(repository, start, repository.resolve(Constants.HEAD).get))
      case x :: xs =>
        repository.cherryPick(x.asInstanceOf[Commit.Raw].raw) match {
          case Left(err) => Left(new GraphRange(this, NonEmptyList.nel(xs.head, xs.tail))) // TODO: unsafe
          case Right(_) => loop(xs)
        }
    }
    loop(range.commits.list.reverse)
  }

  // parent in the target graph
  private def parent(commit: Commit): Commit = {
    val commits = currentThread.commits
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

  def applyCurrentThread: Either[String, ArrangingGraph] = {
    repository.resetHard(start)
    currentThread.perform(repository) match {
      case Left(failure) => Left(failure.toString)
      case Right(thread) =>
        thread.commits.headOption match {
          case None =>
            Left("Unsupported operation: All commits are deleted")
          case Some(commit) =>
            Right(nextGraph(commit.asInstanceOf[Commit.Raw]))
        }
    }
  }

  private def nextGraph(newLast: ObjectId) = new ArrangingGraph(repository, start, newLast)

}

class GraphRange(val graph: ArrangingGraph, val commits: NonEmptyList[Commit]) {
  def first: Commit = {
    commits.head
  }

}
