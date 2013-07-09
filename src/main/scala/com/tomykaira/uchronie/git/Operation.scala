package com.tomykaira.uchronie.git

sealed trait Operation
object Operation {
  case class DeleteOp(commit: Commit) extends Operation {
    override def toString = {
      s"Delete: ${commit.shortId} ${commit.message}"
    }
  }
  case class MoveOp(commits: List[Commit], pos: Int) extends Operation {
    override def toString = {
      s"Move: ${commits.last.shortId}..${commits.head.shortId} to $pos"
    }
  }
  case class RenameOp(commit: Commit, message: String) extends Operation {
    override def toString = {
      s"Change comment: ${commit.shortId} from ${commit.message} to $message"
    }
  }
  case class SquashOp(commits: List[Commit], message: Option[String]) extends Operation {
    override def toString = {
      s"Squash: ${commits.last.shortId}..${commits.head.shortId}"
    }
  }
}
