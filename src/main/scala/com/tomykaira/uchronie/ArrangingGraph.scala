package com.tomykaira.uchronie

import org.eclipse.jgit.lib.{Constants, ObjectId}
import scala.annotation.tailrec
import com.tomykaira.uchronie.git._
import com.tomykaira.uchronie.git.Commit.Raw
import scalaz.NonEmptyList
import scala.Some

class ArrangingGraph(val repository: GitRepository, val start: ObjectId, val last: ObjectId) {
  type OperationResult = Either[String, CommitThread]

  private lazy val startCommit: Commit.Raw = Raw(repository.toCommit(start))
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

  def squashMessage(rows: NonEmptyList[Int]): String =
    rowsToCommits(rows).list.reverse.map(_.message.stripLineEnd).mkString("\n\n")

  def rowsToCommits(rows: NonEmptyList[Int]): NonEmptyList[Commit] =
    rows.map(i => currentThread.commits(i))

  def applyCurrentThread: Either[String, ArrangingGraph] = {
    repository.resetHard(start)
    currentThread.perform(repository) match {
      case Left(failure) => Left(failure.toString)
      case Right(thread) =>
        thread.commits.headOption match {
          case None =>
            Left("Unsupported operation: All commits are deleted")
          case Some(commit) =>
            Right(next(commit.asInstanceOf[Commit.Raw]))
        }
    }
  }

  def next(newLast: ObjectId) = new ArrangingGraph(repository, start, newLast)

  def startEdit(index: TargetRange.Index): IncrementalEditor.Going = {
    val range = commits.slice(0, index + 1).reverse
    val parent = if (index + 1 >= commits.length) startCommit else commits(index + 1)
    IncrementalEditor.startEdit(repository, parent, range)
  }
}