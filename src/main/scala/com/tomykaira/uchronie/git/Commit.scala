package com.tomykaira.uchronie.git

import org.eclipse.jgit.revwalk.RevCommit
import scala.language.implicitConversions

sealed trait Commit {
  def derived(commit: Commit): Boolean
  val message: String
  def simplify: Commit
  def isSimple: Boolean
  def isRaw: Boolean
}
object Commit {
  implicit def commitToRevCommit(commit: Commit): RevCommit = commit.asInstanceOf[Raw].raw
  implicit def revCommitToRawCommit(rev: RevCommit): Commit = Raw(rev)

  case class Pick(previous: Commit) extends Commit {
    def derived(commit: Commit) = this == commit || (previous derived commit)

    val message: String = previous.message

    def simplify =
      previous.simplify match {
        case it @ (Pick(_) | Rename(_,_)) => it
        case it => Pick(it)
      }

    def isSimple = previous.isRaw

    def isRaw = false
  }

  case class Rename(previous: Commit, message: String) extends Commit {
    def derived(commit: Commit) = this == commit || (previous derived commit)

    def simplify =
      previous.simplify match {
        case Pick(c) => Rename(c, message)
        case Rename(c, _) => Rename(c, message)
        case it => Rename(it, message)
      }

    def isSimple = previous.isRaw

    def isRaw = false
  }

  case class Squash(previous: List[Commit], message: String) extends Commit {
    def derived(commit: Commit) = this == commit || previous.exists(_ derived commit)

    def simplify =
      Squash(previous.map(_.simplify), message)

    def isSimple = previous.forall(_.isRaw)

    def isRaw = false
  }

  case class Raw(raw: RevCommit) extends Commit {
    def derived(commit: Commit): Boolean = this == commit

    val message = raw.getFullMessage

    def simplify: Commit = this

    def isSimple = true

    def isRaw = true
  }

  // for testing
  case class DummyCommit(id: Int) extends Commit {
    def derived(commit: Commit) = this == commit

    val message = s"Dummy $id"

    def simplify: Commit = this

    def isSimple = true

    def isRaw = true
  }
}