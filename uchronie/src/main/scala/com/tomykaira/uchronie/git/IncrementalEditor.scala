package com.tomykaira.uchronie.git

import com.tomykaira.uchronie.GitRepository
import scala.annotation.tailrec
import com.tomykaira.uchronie.git.Commit.Raw

sealed trait IncrementalEditor {
  val repository: GitRepository

  val commits: List[Raw]

  val head: Raw
}

object IncrementalEditor {

  def startEdit(repository: GitRepository, parent: Raw, commits: List[Raw]): Going = {
    repository.resetHard(commits.head.raw)
    repository.resetSoft(parent.raw)

    Going(repository, commits.tail, commits.head)
  }

  case class Going(repository: GitRepository, commits: List[Raw], head: Raw) extends IncrementalEditor {
    def continue: IncrementalEditor = {
      if (!repository.isClean)
        return this

      @tailrec
      def loop(commits: List[Raw], current: Raw): IncrementalEditor = commits match {
        case Nil => done(current)
        case x :: xs =>
          repository.cherryPick(x.raw) match {
            case Left(err) => failed(xs, current)
            case Right(next) => loop(xs, Raw(next))
          }
      }

      loop(commits, Raw(repository.head))
    }

    private def failed(rest: List[Raw], next: Raw) =
      Going(repository, rest, head)

    private def done(next: Raw) =
      Done(repository, next)
  }

  case class Done(repository: GitRepository, head: Raw) extends IncrementalEditor {
    val commits: List[Raw] = List()
  }
}