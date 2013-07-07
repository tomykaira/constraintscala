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
    case _ => Right(this)
  }

  private def pick(cs: List[VirtualCommit]) = cs map VirtualCommit.Pick
  private def move(cs: List[VirtualCommit]) = cs map VirtualCommit.Move
}
