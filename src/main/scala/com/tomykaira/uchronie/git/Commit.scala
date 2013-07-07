package com.tomykaira.uchronie.git

import org.eclipse.jgit.revwalk.RevCommit
import scala.language.implicitConversions

object Commit {
  implicit def commitToRevCommit(commit: Commit): RevCommit = commit.raw
  implicit def revCommitToRawCommit(rev: RevCommit): Commit = RawCommit(rev)
}
sealed trait VirtualCommit {
}
sealed trait Commit extends VirtualCommit {
  val raw: RevCommit
}
object VirtualCommit {
  case class Pick(previous: VirtualCommit) extends VirtualCommit
  case class Rename(previous: VirtualCommit, message: String) extends VirtualCommit
  case class Move(previous: VirtualCommit) extends VirtualCommit
  case class Squash(previous: List[VirtualCommit], message: String) extends VirtualCommit

  // for testing
  case class DummyCommit(id: Int) extends VirtualCommit
}
case class RawCommit(raw: RevCommit) extends Commit
case class Picked(raw: RevCommit, previous: Commit) extends Commit
case class Renamed(raw: RevCommit, previous: Commit) extends Commit
case class Moved(raw: RevCommit, previous: Commit) extends Commit
case class Squashed(raw: RevCommit, previous: List[Commit]) extends Commit
