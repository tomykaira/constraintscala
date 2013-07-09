package com.tomykaira.uchronie.git

import scalaz.NonEmptyList

sealed trait Operation

object Operation {
  type Index = Int
  
  case class DeleteOp(commit: Int) extends Operation

  case class MoveOp(commits: List[Int], pos: Int) extends Operation

  case class RenameOp(commit: Int, message: String) extends Operation

  case class SquashOp(commits: List[Int], message: Option[String]) extends Operation
}
