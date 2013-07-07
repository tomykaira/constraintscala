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

  def applyOperation(op: Operation): Either[CommitThread.Error, CommitThread] = op match {
    case RenameOp(commit, message) =>
      commits.indexOf(commit) match {
        case -1 => Left(CommitThread.CommitNotFound(this, op, commit))
        case index: Int =>
          val newCommits: List[VirtualCommit] =
            pick(commits.take(index)) ++ (VirtualCommit.Rename(commit, message) :: commits.drop(index + 1))
          Right(CommitThread.fromVirtualCommits(newCommits))
      }
    case MoveOp(targets, pos) =>
      val indices = targets.map(commits.indexOf(_))
      val notFound = indices.indexOf(-1)
      if (notFound != -1)
        Left(CommitThread.CommitNotFound(this, op, targets(notFound)))
      else {
        val lastTarget = indices.max
        val picked = pick(commits.take(lastTarget)) ++ commits.drop(lastTarget)
        val newCommits = picked.take(pos).filterNot(p => targets.exists(t => p derived t)) ++
          move(targets) ++
          picked.drop(pos).filterNot(p => targets.exists(t => p derived t))
        Right(CommitThread.fromVirtualCommits(newCommits))
      }
    case SquashOp(targets, message) =>
      if (targets.isEmpty) {
        Right(this)
      } else if (commits.containsSlice(targets)) {
        val firstIndex = commits.indexOf(targets.head)
        val lastIndex = firstIndex + targets.length
        val newMessage = message.getOrElse(targets.reverse.map(_.message.stripLineEnd).mkString("\n\n"))
        val newCommits = pick(commits.take(firstIndex)) ++ (VirtualCommit.Squash(targets, newMessage) :: commits.drop(lastIndex))
        Right(CommitThread.fromVirtualCommits(newCommits))
      } else {
        Left(CommitThread.NotSequentialSlice(this, op))
      }
    case _ => Right(this)
  }

  private def pick(cs: List[VirtualCommit]) = cs map VirtualCommit.Pick
  private def move(cs: List[VirtualCommit]) = cs map VirtualCommit.Move
}
