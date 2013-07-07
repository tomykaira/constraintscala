package com.tomykaira.uchronie.git

sealed trait Operation
case class DeleteOp(commit: VirtualCommit) extends Operation
case class MoveOp(commits: List[VirtualCommit], pos: Int) extends Operation
case class RenameOp(commit: VirtualCommit, message: String) extends Operation
case class SquashOp(commits: List[VirtualCommit], message: Option[String]) extends Operation
