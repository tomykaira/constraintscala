package com.tomykaira.uchronie.ui

import com.tomykaira.uchronie.git.Commit
import com.tomykaira.uchronie.GitRepository

case class CommitDecorator(commit: Commit) {
  val oneLineMessage: String = {
    val message = commit.message
    val lineEnd = message.indexOf('\n')
    if (lineEnd == -1)
      message
    else
      message.take(lineEnd)
  }

  def tableRow: Array[AnyRef] = Array(commit.shortId, oneLineMessage)

  def shortDescription: String = commit.shortId + " " + oneLineMessage

  def diff(repository: GitRepository): List[DiffDecorator] = commit match {
    case raw: Commit.Raw => oneFile(repository, raw)
    case Commit.Pick(raw) => oneFile(repository, raw)
    case Commit.Rename(raw, _) => oneFile(repository, raw)
    case sq: Commit.Squash => sq.previous flatMap { r => oneFile(repository, r) }
  }

  private def oneFile(repository: GitRepository, raw: Commit.Raw) = {
    val diffs = repository.diff(raw.raw)
    DiffDecorator.All(new CommitDecorator(raw).shortDescription, diffs) :: (diffs map DiffDecorator.Each)
  }
}
