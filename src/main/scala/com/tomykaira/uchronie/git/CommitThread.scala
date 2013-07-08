package com.tomykaira.uchronie.git

import org.eclipse.jgit.revwalk.RevCommit
import scala.collection.mutable.ListBuffer
import com.tomykaira.uchronie.GitRepository

object CommitThread {
  def fromCommits(cs: List[Commit]): CommitThread =
    new CommitThread {
      val commits: List[Commit] = cs
    }

  def fromRevCommits(cs: List[RevCommit]): CommitThread =
    new CommitThread {
      val commits: List[Commit] = cs.map(Commit.revCommitToRawCommit)
    }

  sealed trait Error {
    val thread: CommitThread
    val operation: Operation
  }
  case class CommitNotFound(thread: CommitThread, operation: Operation, commit: Commit) extends Error
  case class NotSequentialSlice(thread: CommitThread, operation: Operation) extends Error
}

trait CommitThread {
  val commits: List[Commit]
  type OperationResult = Either[CommitThread.Error, CommitThread]

  def isSimple: Boolean = commits.forall(_.isSimple)

  def applyOperation(op: Operation): OperationResult = op match {
    case Operation.RenameOp(target, message) =>
      withTargetIndex(target, op).right.flatMap { index =>
        result(pick(commits.take(index)) ++ (Commit.Rename(target, message) :: commits.drop(index + 1)))
      }
    case Operation.DeleteOp(target) =>
      withTargetIndex(target, op).right.flatMap { index =>
        result(pick(commits.take(index)) ++ commits.drop(index + 1))
      }
    case Operation.MoveOp(targets, pos) =>
      if (targets.isEmpty)
        return Right(this)
      indicesInList(targets, op).right.flatMap { indices =>
        val lastTarget = indices.max
        val picked = pick(commits.take(lastTarget)) ++ commits.drop(lastTarget)
        result(picked.take(pos).filterNot(p => targets.exists(t => p derived t)) ++
          pick(targets) ++
          picked.drop(pos).filterNot(p => targets.exists(t => p derived t)))
      }
    case Operation.SquashOp(targets, message) =>
      if (targets.isEmpty) {
        Right(this)
      } else if (commits.containsSlice(targets)) {
        val firstIndex = commits.indexOf(targets.head)
        val lastIndex = firstIndex + targets.length
        val newMessage = message.getOrElse(targets.reverse.map(_.message.stripLineEnd).mkString("\n\n"))
        result(pick(commits.take(firstIndex)) ++ (Commit.Squash(targets, newMessage) :: commits.drop(lastIndex)))
      } else {
        Left(CommitThread.NotSequentialSlice(this, op))
      }
  }

  private def indicesInList(targets: List[Commit], op: Operation): Either[CommitThread.Error, List[Int]] = {
    val indices = targets.map(commits.indexOf(_))
    val notFound = indices.indexOf(-1)
    if (notFound != -1) {
      Left(CommitThread.CommitNotFound(this, op, targets(notFound)))
    } else {
      Right(indices)
    }
  }

  private def withTargetIndex(target: Commit, op: Operation) =
    commits.indexOf(target) match {
      case -1 => Left(CommitThread.CommitNotFound(this, op, target))
      case index: Int => Right(index)
    }

  private def result(commits: List[Commit]) =
    Right(CommitThread.fromCommits(commits.map(_.simplify)))

  private def pick(cs: List[Commit]) = cs map Commit.Pick

  /**
   * The caller must reset to the start commit of this thread before calling
   * @param  repository Current processing git repository
   * @return Error from OperationCommit.perform on failure, new CommitThread
   *         with Raw commits on success
   */
  def perform(repository: GitRepository): Either[Commit.Error, CommitThread] = {
    val newCommits = commits.foldRight[Either[Commit.Error, List[Commit]]](Right(List()))((current, news) =>
      current match {
        case commit: Commit.Operational =>
          news.right.flatMap(list => commit.perform(repository).right.map(_ :: list))
        case concrete: Commit.Concrete =>
          news.right.map(concrete :: _)
      }
    )
    newCommits.right.map(CommitThread.fromCommits)
  }
}
