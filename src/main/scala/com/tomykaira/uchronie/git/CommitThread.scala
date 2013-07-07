package com.tomykaira.uchronie.git

import org.eclipse.jgit.revwalk.RevCommit

object CommitThread {
  def fromVirtualCommits(cs: List[VirtualCommit]): CommitThread =
    new CommitThread {
      val commits: List[VirtualCommit] = cs
    }

  def fromRevCommits(cs: List[RevCommit]): CommitThread =
    new CommitThread {
      val commits: List[VirtualCommit] = cs.map(Commit.revCommitToRawCommit)
    }

  sealed trait Error {
    val thread: CommitThread
    val operation: Operation
  }
  case class CommitNotFound(thread: CommitThread, operation: Operation, commit: VirtualCommit) extends Error
  case class NotSequentialSlice(thread: CommitThread, operation: Operation) extends Error
}

trait CommitThread {
  val commits: List[VirtualCommit]
  type OperationResult = Either[CommitThread.Error, CommitThread]

  def applyOperation(op: Operation): OperationResult = op match {
    case RenameOp(target, message) =>
      withTargetIndex(target, op).right.flatMap { index =>
        result(pick(commits.take(index)) ++ (VirtualCommit.Rename(target, message) :: commits.drop(index + 1)))
      }
    case DeleteOp(target) =>
      withTargetIndex(target, op).right.flatMap { index =>
        result(pick(commits.take(index)) ++ commits.drop(index + 1))
      }
    case MoveOp(targets, pos) =>
      if (targets.isEmpty)
        return Right(this)
      indicesInList(targets, op).right.flatMap { indices =>
        val lastTarget = indices.max
        val picked = pick(commits.take(lastTarget)) ++ commits.drop(lastTarget)
        result(picked.take(pos).filterNot(p => targets.exists(t => p derived t)) ++
          move(targets) ++
          picked.drop(pos).filterNot(p => targets.exists(t => p derived t)))
      }
    case SquashOp(targets, message) =>
      if (targets.isEmpty) {
        Right(this)
      } else if (commits.containsSlice(targets)) {
        val firstIndex = commits.indexOf(targets.head)
        val lastIndex = firstIndex + targets.length
        val newMessage = message.getOrElse(targets.reverse.map(_.message.stripLineEnd).mkString("\n\n"))
        result(pick(commits.take(firstIndex)) ++ (VirtualCommit.Squash(targets, newMessage) :: commits.drop(lastIndex)))
      } else {
        Left(CommitThread.NotSequentialSlice(this, op))
      }
  }

  private def indicesInList(targets: List[VirtualCommit], op: Operation): Either[CommitThread.Error, List[Int]] = {
    val indices = targets.map(commits.indexOf(_))
    val notFound = indices.indexOf(-1)
    if (notFound != -1) {
      Left(CommitThread.CommitNotFound(this, op, targets(notFound)))
    } else {
      Right(indices)
    }
  }

  private def withTargetIndex(target: VirtualCommit, op: Operation) =
    commits.indexOf(target) match {
      case -1 => Left(CommitThread.CommitNotFound(this, op, target))
      case index: Int => Right(index)
    }

  private def result(commits: List[VirtualCommit]) =
    Right(CommitThread.fromVirtualCommits(commits))

  private def pick(cs: List[VirtualCommit]) = cs map VirtualCommit.Pick
  private def move(cs: List[VirtualCommit]) = cs map VirtualCommit.Move
}
