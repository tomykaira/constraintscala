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

  def rawPreviousCommit(commit: Commit): Either[Commit.Error, Commit.Raw] = commit match {
    case r: Commit.Raw => Right(r)
    case _: OperationCommit => Left(Commit.NotSimple())
    case _: Commit.DummyCommit => Left(Commit.PreviousIsDummy())
  }

  def pick(raw: Commit.Raw, repository: GitRepository): Either[Commit.Error, Commit.Raw] =
    repository.cherryPick(raw) match {
      case Right(newCommit) => Right(Commit.Raw(newCommit))
      case Left(error) => Left(Commit.Failed(error))
    }
}
object Commit {
  implicit def commitToRevCommit(commit: Commit): RevCommit = commit.asInstanceOf[Raw].raw
  implicit def revCommitToRawCommit(rev: RevCommit): Commit = Raw(rev)

  sealed trait Error
  case class NotSimple() extends Error
  case class PreviousIsDummy() extends Error
  case class EmptySquash() extends Error
  case class Failed(reason: String) extends Error

  case class Pick(previous: Commit) extends OperationCommit {
    def perform(repository: GitRepository): Either[Commit.Error, Raw] =
      rawPreviousCommit(previous).right.flatMap(pick(_, repository))

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
    def perform(repository: GitRepository): Either[Commit.Error, Raw] =
      for {
        raw <- rawPreviousCommit(previous).right
        _ <- pick(raw, repository).right
        amended <- Right(repository.amendMessage(message)).right
      } yield Raw(amended)

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
    type PerformanceResult = Either[Commit.Error, Raw]
    def perform(repository: GitRepository): PerformanceResult =
      previous.reverse match {
        case Nil => Left(EmptySquash())
        case head :: rest =>
          for {
            rawHead <- rawPreviousCommit(head).right
            newHead <- pick(rawHead, repository).right
            _ <- rest.foldRight[PerformanceResult](Right(newHead)) { (commit, prev) =>
              prev.right.flatMap(_ => rawPreviousCommit(commit)).right.flatMap(pick(_, repository))
            }.right
            _ <- Right(repository.resetSoft(newHead.getParent(0))).right
            lastCommit <- Right(repository.commit(message)).right
          } yield Raw(lastCommit)
      }

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