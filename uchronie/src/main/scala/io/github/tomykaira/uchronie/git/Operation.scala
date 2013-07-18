package io.github.tomykaira.uchronie.git

import io.github.tomykaira.uchronie.TargetRange.Index
import io.github.tomykaira.uchronie.TargetRange

sealed trait Operation

object Operation {
  case class DeleteOp(commit: Index) extends Operation

  case class MoveOp(commits: TargetRange, pos: Index) extends Operation

  case class RenameOp(commit: Index, message: String) extends Operation

  case class SquashOp(commits: TargetRange, message: Option[String]) extends Operation
}
