package com.tomykaira.uchronie.git

import org.eclipse.jgit.revwalk.RevCommit
import scala.language.implicitConversions

object Commit {
  implicit def commitToRevCommit(commit: Commit): RevCommit = commit.raw
  implicit def revCommitToRawCommit(rev: RevCommit): Commit = RawCommit(rev)
}
sealed trait VirtualCommit {
  def derived(commit: VirtualCommit): Boolean
  val message: String
  def simplify: VirtualCommit
}
sealed trait Commit extends VirtualCommit {
  val raw: RevCommit
  val message = raw.getFullMessage
  def simplify: VirtualCommit = this
}
object VirtualCommit {
  case class Pick(previous: VirtualCommit) extends VirtualCommit {
    def derived(commit: VirtualCommit) = this == commit || (previous derived commit)

    val message: String = previous.message

    override def simplify: VirtualCommit =
      previous.simplify match {
        case it @ (Pick(_) | Rename(_,_)) => it
        case it => Pick(it)
      }
  }

  case class Rename(previous: VirtualCommit, message: String) extends VirtualCommit {
    def derived(commit: VirtualCommit) = this == commit || (previous derived commit)

    override def simplify: VirtualCommit =
      previous.simplify match {
        case Pick(c) => Rename(c, message)
        case Rename(c, _) => Rename(c, message)
        case it => Rename(it, message)
      }
  }

  case class Squash(previous: List[VirtualCommit], message: String) extends VirtualCommit {
    def derived(commit: VirtualCommit) = this == commit || previous.exists(_ derived commit)

    override def simplify: VirtualCommit =
      Squash(previous.map(_.simplify), message)
  }

  // for testing
  case class DummyCommit(id: Int) extends VirtualCommit{
    def derived(commit: VirtualCommit) = this == commit
    val message = s"Dummy $id"
    def simplify: VirtualCommit = this
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
