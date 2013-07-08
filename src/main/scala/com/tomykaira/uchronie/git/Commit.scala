package com.tomykaira.uchronie.git

import org.eclipse.jgit.revwalk.RevCommit
import scala.language.implicitConversions
import com.tomykaira.uchronie.GitRepository

sealed trait Commit {
  def derived(commit: Commit): Boolean
  val message: String
  def simplify: Commit
  def isSimple: Boolean
  def isRaw: Boolean
}
sealed trait ConcreteCommit extends Commit {
  def simplify: Commit = this
  def isSimple: Boolean = true
  def isRaw: Boolean = true
}
sealed trait OperationCommit extends Commit {
  def isRaw: Boolean = false
}
object Commit {
  implicit def commitToRevCommit(commit: Commit): RevCommit = commit.asInstanceOf[Raw].raw
  implicit def revCommitToRawCommit(rev: RevCommit): Commit = Raw(rev)

  sealed trait Error
  case class NotSimple() extends Error
  case class PreviousIsDummy() extends Error
  case class Failed(reason: String) extends Error

  case class Pick(previous: Commit) extends OperationCommit {
    def perform(repository: GitRepository): Either[Commit.Error, Raw] =
      previous match {
        case Raw(c) => repository.cherryPick(c) match {
          case Right(newCommit) => Right(Raw(newCommit))
          case Left(error) => Left(Failed(error))
        }
        case _: OperationCommit => Left(NotSimple())
        case _: DummyCommit => Left(PreviousIsDummy())
      }

    def derived(commit: Commit) = this == commit || (previous derived commit)

    val message: String = previous.message

    def simplify =
      previous.simplify match {
        case it @ (Pick(_) | Rename(_,_)) => it
        case it => Pick(it)
      }

    def isSimple = previous.isRaw
  }

  case class Rename(previous: Commit, message: String) extends OperationCommit {
    def derived(commit: Commit) = this == commit || (previous derived commit)

    def simplify =
      previous.simplify match {
        case Pick(c) => Rename(c, message)
        case Rename(c, _) => Rename(c, message)
        case it => Rename(it, message)
      }

    def isSimple = previous.isRaw
  }

  case class Squash(previous: List[Commit], message: String) extends OperationCommit {
    def derived(commit: Commit) = this == commit || previous.exists(_ derived commit)

    def simplify =
      Squash(previous.map(_.simplify), message)

    def isSimple = previous.forall(_.isRaw)
  }

  case class Raw(raw: RevCommit) extends ConcreteCommit {
    def derived(commit: Commit): Boolean = this == commit

    val message = raw.getFullMessage
  }

  // for testing
  case class DummyCommit(id: Int) extends ConcreteCommit {
    def derived(commit: Commit) = this == commit

    val message = s"Dummy $id"
  }
}