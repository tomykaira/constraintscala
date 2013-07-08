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
object Commit {
  implicit def commitToRevCommit(commit: Commit): RevCommit = commit.asInstanceOf[Raw].raw
  implicit def revCommitToRawCommit(rev: RevCommit): Commit = Raw(rev)

  sealed trait Error
  case class NotSimple() extends Error
  case class PreviousIsDummy() extends Error
  case class EmptySquash() extends Error
  case class Failed(reason: String) extends Error

  sealed trait Concrete extends Commit {
    def derived(commit: Commit): Boolean = this == commit
    def simplify: Commit = this
    def isSimple: Boolean = true
    def isRaw: Boolean = true
  }

  sealed trait Operational extends Commit {
    def isRaw: Boolean = false

    type PerformanceResult = Either[Error, Raw]

    def perform(repository: GitRepository): PerformanceResult

    def rawPreviousCommit(commit: Commit): Either[Error, Raw] = commit match {
      case r: Raw => Right(r)
      case _: Operational => Left(NotSimple())
      case _: DummyCommit => Left(PreviousIsDummy())
    }

    protected def pick(raw: Commit.Raw, repository: GitRepository): Either[Error, Raw] =
      repository.cherryPick(raw) match {
        case Right(newCommit) => Right(Raw(newCommit))
        case Left(error) => Left(Failed(error))
      }
  }

  case class Pick(previous: Commit) extends Operational {
    def perform(repository: GitRepository) =
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

  case class Rename(previous: Commit, message: String) extends Operational {
    def perform(repository: GitRepository) =
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

  case class Squash(previous: List[Commit], message: String) extends Operational {
    def perform(repository: GitRepository) =
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

  case class Raw(raw: RevCommit) extends Concrete {
    val message = raw.getFullMessage
  }

  // for testing
  case class DummyCommit(id: Int) extends Concrete {
    val message = s"Dummy $id"
  }
}