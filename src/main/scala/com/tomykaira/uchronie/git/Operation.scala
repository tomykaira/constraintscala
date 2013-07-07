package com.tomykaira.uchronie.git

sealed trait Operation
object Operation {
  case class DeleteOp(commit: Commit) extends Operation
  case class MoveOp(commits: List[Commit], pos: Int) extends Operation
  case class RenameOp(commit: Commit, message: String) extends Operation
  case class SquashOp(commits: List[Commit], message: Option[String]) extends Operation
}
