package com.tomykaira.uchronie

import com.tomykaira.uchronie.git.Commit
import org.eclipse.jgit.diff.DiffEntry

case class CommitDecorator(commit: Commit) {
  def tableRow: Array[AnyRef] =
    Array(commit.shortId, commit.message.split("\n").head)

  def diff(repository: GitRepository): Option[List[DiffEntry]] = commit match {
    case Commit.Raw(raw) => Some(repository.diff(raw))
    case Commit.Pick(Commit.Raw(raw)) => Some(repository.diff(raw))
    case Commit.Rename(Commit.Raw(raw), _) => Some(repository.diff(raw))
    case _ => None
  }

}
