package com.tomykaira.uchronie.git

import org.eclipse.jgit.lib.ObjectId
import com.tomykaira.uchronie.{TargetRange, GitRepository}
import com.tomykaira.uchronie.git.Commit.Raw

sealed trait ArrangingGraph {
  val repository: GitRepository

  val start: Commit.Raw

  val last: Commit.Raw

  val thread: CommitThread

  val history: List[Operation]

  lazy val commits = thread.commits

  def apply(index: TargetRange.Index): Option[Commit] = {
    if (commits.indices.contains(index))
      Some(commits(index))
    else
      None
  }

  def transit(op: Operation): ArrangingGraph.Modified = {
    val from = this
    new ArrangingGraph.Modified {
      val thread = from.thread.applyOperation(op)
      val history = op :: from.history
      val previous = from
    }
  }

  def rollback: ArrangingGraph.Clean = {
    repository.resetHard(last)
    ArrangingGraph.renew(this, last)
  }

  def squashMessage(rows: List[Int]): String =
    rowsToCommits(rows).reverse.map(_.message.stripLineEnd).mkString("\n\n")

  def rowsToCommits(rows: List[Int]): List[Commit] =
    rows.foldRight(List[Commit]())((i, r) => apply(i) match {
      case Some(c) => c :: r
      case None => r }
    )
}

object ArrangingGraph {
  sealed trait Clean extends ArrangingGraph {
    lazy private val rawCommits = repository.listCommits(start, last)
    lazy val thread: CommitThread = CommitThread.fromRevCommits(rawCommits)
    val history: List[Operation] = List()

    def startEdit(index: TargetRange.Index): IncrementalEditor.Going = {
      val commits = rawCommits.map(Raw)
      val range = commits.slice(0, index + 1).reverse
      val parent = if (index + 1 >= commits.length) start else commits(index + 1)
      IncrementalEditor.startEdit(repository, parent, range)
    }
  }

  sealed trait Modified extends ArrangingGraph {
    val previous: ArrangingGraph
    lazy val repository: GitRepository = previous.repository
    lazy val start = previous.start
    lazy val last = previous.last

    def applyCurrentThread: Either[String, ArrangingGraph.Clean] = {
      repository.resetHard(start)
      thread.perform(repository) match {
        case Left(failure) => Left(failure.toString)
        case Right(result) =>
          result.headOption match {
            case None =>
              Left("Unsupported operation: All commits are deleted")
            case Some(commit) =>
              Right(renew(this, commit))
          }
      }
    }
  }

  def startUp(repo: GitRepository, startRef: ObjectId, lastRef: ObjectId): ArrangingGraph.Clean = {
    new Clean {
      val repository: GitRepository = repo
      val start: Raw = Raw(repository.toCommit(startRef))
      val last: Raw = Raw(repository.toCommit(lastRef))

      repository.resetHard(last)
    }
  }

  def renew(base: ArrangingGraph, next: Raw): ArrangingGraph.Clean = {
    new Clean {
      val repository: GitRepository = base.repository
      val start: Raw = base.start
      val last: Raw = next
    }
  }

}