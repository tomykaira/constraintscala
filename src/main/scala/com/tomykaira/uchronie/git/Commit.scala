package com.tomykaira.uchronie.git

import org.eclipse.jgit.revwalk.RevCommit
import org.eclipse.jgit.lib.ObjectId
import scala.language.implicitConversions
import com.tomykaira.uchronie.{CherryPickFailure, GitRepository}
import scalaz.NonEmptyList

sealed trait Commit {
  def derived(commit: Commit): Boolean
  val message: String
  val shortId: String
  val id: ObjectId
  def simplify: Commit
}
object Commit {
  implicit def revCommitToRawCommit(rev: RevCommit): Commit = Raw(rev)

  implicit def rawCommitToRevCommit(raw: Commit.Raw): RevCommit = raw.raw

  class NotSimplifiedException extends RuntimeException("Defeat: maybe simplify call is forgot")

  class DummyCommitException extends RuntimeException("Defeat: DummyCommit is for testing")

  sealed trait Concrete extends Commit {
    def derived(commit: Commit): Boolean = this == commit
    def simplify: Commit = this
  }

  sealed trait Operational extends Commit {
    type PerformanceResult = Either[CherryPickFailure, Raw]

    def perform(repository: GitRepository): PerformanceResult

    val id = ObjectId.zeroId()

    val shortId = id.abbreviate(7).name()

    def rawPreviousCommit(commit: Commit): Raw = commit match {
      case r: Raw => r
      case _: Operational => throw new NotSimplifiedException
      case _: DummyCommit => throw new DummyCommitException
    }

    protected def pickPrevious(commit: Commit, repository: GitRepository): Either[CherryPickFailure, Raw] =
      pick(rawPreviousCommit(commit), repository)

    protected def pick(raw: Commit.Raw, repository: GitRepository): Either[CherryPickFailure, Raw] =
      repository.cherryPick(raw).right.map(Raw)
  }

  case class Pick(previous: Commit) extends Operational {
    def perform(repository: GitRepository) =
      pickPrevious(previous, repository)

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
        _ <- pickPrevious(previous, repository).right
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

  case class Squash(previous: NonEmptyList[Commit], message: String) extends Operational {
    def perform(repository: GitRepository) = {
      val reversed = previous.reverse
      for {
        newHead <- pickPrevious(reversed.head, repository).right
        _ <- reversed.tail.foldRight[PerformanceResult](Right(newHead)) { (commit, prev) =>
          prev.right.flatMap(_ => pickPrevious(commit, repository))
        }.right
        _ <- Right(repository.resetSoft(newHead.raw.getParent(0))).right
        lastCommit <- Right(repository.commit(message)).right
      } yield Raw(lastCommit)
    }

    def derived(commit: Commit) = this == commit || previous.list.exists(_ derived commit)

    def simplify = {
      val news = previous.list.foldRight[List[Commit]](List()) { (current, list) =>
        current.simplify match {
          case Pick(c) => c :: list
          case Rename(c, _) => c :: list
          case Squash(cs, _) => cs.list ++ list
          case it: Concrete => it :: list
        }
      }
      Squash(NonEmptyList.nel(news.head, news.tail), message)
    }
  }

  case class Raw(raw: RevCommit) extends Concrete {
    val message = raw.getFullMessage

    val id = raw.getId

    val shortId = id.abbreviate(7).name()
  }

  // for testing
  case class DummyCommit(i: Int) extends Concrete {
    val message = s"Dummy $i"

    val id = ObjectId.fromRaw(Array(i, 0, 0, 0, 0))

    val shortId = id.abbreviate(7).name()
  }
}