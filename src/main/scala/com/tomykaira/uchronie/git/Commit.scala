package com.tomykaira.uchronie.git

import org.eclipse.jgit.revwalk.RevCommit
import scala.language.implicitConversions
import com.tomykaira.uchronie.GitRepository

sealed trait Commit {
  def derived(commit: Commit): Boolean
  val message: String
  def simplify: Commit
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
  }

  sealed trait Operational extends Commit {
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
        case it: Operational => it
        case it: Concrete => Pick(it)
      }
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
        case Squash(cs, _) => Squash(cs, message)
        case it: Concrete => Rename(it, message)
      }
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

    def simplify = {
      val news = previous.foldRight[List[Commit]](List()) { (current, list) =>
        current.simplify match {
          case Pick(c) => c :: list
          case Rename(c, _) => c :: list
          case Squash(cs, _) => cs ++ list
          case it: Concrete => it :: list
        }
      }
      Squash(news, message)
    }
  }

  case class Raw(raw: RevCommit) extends Concrete {
    val message = raw.getFullMessage
  }

  // for testing
  case class DummyCommit(id: Int) extends Concrete {
    val message = s"Dummy $id"
  }
}