package com.tomykaira.uchronie.git

import org.eclipse.jgit.revwalk.RevCommit
import scala.language.implicitConversions

sealed trait Commit {
  def derived(commit: Commit): Boolean
  val message: String
  def simplify: Commit
}
object Commit {
  implicit def commitToRevCommit(commit: Commit): RevCommit = commit.asInstanceOf[Raw].raw
  implicit def revCommitToRawCommit(rev: RevCommit): Commit = Raw(rev)

  case class Pick(previous: Commit) extends Commit {
    def derived(commit: Commit) = this == commit || (previous derived commit)

    val message: String = previous.message

    override def simplify: Commit =
      previous.simplify match {
        case it @ (Pick(_) | Rename(_,_)) => it
        case it => Pick(it)
      }
  }

  case class Rename(previous: Commit, message: String) extends Commit {
    def derived(commit: Commit) = this == commit || (previous derived commit)

    override def simplify: Commit =
      previous.simplify match {
        case Pick(c) => Rename(c, message)
        case Rename(c, _) => Rename(c, message)
        case it => Rename(it, message)
      }
  }

  case class Squash(previous: List[Commit], message: String) extends Commit {
    def derived(commit: Commit) = this == commit || previous.exists(_ derived commit)

    override def simplify: Commit =
      Squash(previous.map(_.simplify), message)
  }

  case class Raw(raw: RevCommit) extends Commit {
    val message = raw.getFullMessage
    def simplify: Commit = this
    def derived(commit: Commit): Boolean = this == commit
  }

  // for testing
  case class DummyCommit(id: Int) extends Commit {
    def derived(commit: Commit) = this == commit
    val message = s"Dummy $id"
    def simplify: Commit = this
  }
}