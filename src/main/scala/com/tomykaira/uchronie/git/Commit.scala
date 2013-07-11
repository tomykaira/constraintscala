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
}
object Commit {
  implicit def revCommitToRawCommit(rev: RevCommit): Commit = Raw(rev)

  implicit def rawCommitToRevCommit(raw: Commit.Raw): RevCommit = raw.raw

  sealed trait Operational extends Commit {
    type PerformanceResult = Either[CherryPickFailure, Raw]

    def perform(repository: GitRepository): PerformanceResult

    val id = ObjectId.zeroId()

    val shortId = id.abbreviate(7).name()

    protected def pick(raw: Commit.Raw, repository: GitRepository): Either[CherryPickFailure, Raw] =
      repository.cherryPick(raw).right.map(Raw)
  }

  case class Pick(previous: Raw) extends Operational {
    def perform(repository: GitRepository) =
      pick(previous, repository)

    def derived(commit: Commit) = this == commit || (previous derived commit)

    val message: String = previous.message
  }

  object Pick {
    def apply(previous: Commit): Operational = previous match {
      case it: Operational => it
      case it: Raw => Pick(it)
    }
  }

  case class Rename(previous: Raw, message: String) extends Operational {
    def perform(repository: GitRepository) =
      for {
        _ <- pick(previous, repository).right
        amended <- Right(repository.amendMessage(message)).right
      } yield Raw(amended)

    def derived(commit: Commit) = this == commit || (previous derived commit)
  }

  object Rename {
    def apply(previous: Commit, message: String): Operational = previous match {
      case Pick(c) => Rename(c, message)
      case Rename(c, _) => Rename(c, message)
      case Squash(cs, _) => Squash(cs, message)
      case it: Raw => Rename(it, message)
    }
  }

  case class Squash(impurePrevious: NonEmptyList[Commit], message: String) extends Operational {
    /** Raw only previous list */
    val previous: List[Raw] = impurePrevious.list.foldRight(List[Raw]()) { (current, list) =>
      current match {
        case Pick(c) => c :: list
        case Rename(c, _) => c :: list
        case sq: Squash => sq.previous ++ list
        case it: Raw => it :: list
      }
    }

    def perform(repository: GitRepository) = {
      val reversed = previous.reverse
      for {
        newHead <- pick(reversed.head, repository).right
        _ <- reversed.tail.foldLeft[PerformanceResult](Right(newHead)) { (prev, commit) =>
          prev.right.flatMap(_ => pick(commit, repository))
        }.right
        _ <- Right(repository.resetSoft(newHead.raw.getParent(0))).right
        lastCommit <- Right(repository.commit(message)).right
      } yield Raw(lastCommit)
    }

    def derived(commit: Commit) = this == commit || previous.exists(_ derived commit)
  }

  case class Raw(raw: RevCommit) extends Commit {
    val message = raw.getFullMessage

    val id = raw.getId

    val shortId = id.abbreviate(7).name()

    def derived(commit: Commit): Boolean = this == commit

    def simplify: Commit = this
  }
}