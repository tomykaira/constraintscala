package com.tomykaira.uchronie.git

import org.eclipse.jgit.revwalk.RevCommit
import scala.language.implicitConversions

object Commit {
  implicit def commitToRevCommit(commit: Commit): RevCommit = commit.raw
  implicit def revCommitToRawCommit(rev: RevCommit): Commit = RawCommit(rev)
}
sealed trait VirtualCommit {
  def derived(commit: VirtualCommit): Boolean
}
sealed trait Commit extends VirtualCommit {
  val raw: RevCommit
}
object VirtualCommit {
  case class Pick(previous: VirtualCommit) extends VirtualCommit {
    def derived(commit: VirtualCommit) = this == commit || (previous derived commit)
  }
  case class Rename(previous: VirtualCommit, message: String) extends VirtualCommit {
    def derived(commit: VirtualCommit) = this == commit || (previous derived commit)
  }
  case class Move(previous: VirtualCommit) extends VirtualCommit {
    def derived(commit: VirtualCommit) = this == commit || (previous derived commit)
  }
  case class Squash(previous: List[VirtualCommit], message: String) extends VirtualCommit {
    def derived(commit: VirtualCommit) = this == commit || previous.exists(_ derived commit)
  }

  // for testing
  case class DummyCommit(id: Int) extends VirtualCommit{
    def derived(commit: VirtualCommit) = this == commit
  }
}
case class RawCommit(raw: RevCommit) extends Commit {
  def derived(commit: VirtualCommit): Boolean = this == commit
}
case class Picked(raw: RevCommit, previous: Commit) extends Commit {
  def derived(commit: VirtualCommit) = this == commit || (previous derived commit)
}
case class Renamed(raw: RevCommit, previous: Commit) extends Commit {
  def derived(commit: VirtualCommit) = this == commit || (previous derived commit)
}
case class Moved(raw: RevCommit, previous: Commit) extends Commit {
  def derived(commit: VirtualCommit) = this == commit || (previous derived commit)
}
case class Squashed(raw: RevCommit, previous: List[Commit]) extends Commit {
  def derived(commit: VirtualCommit) = this == commit || previous.exists(_ derived commit)
}
