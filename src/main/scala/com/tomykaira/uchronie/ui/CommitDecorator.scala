package com.tomykaira.uchronie.ui

import com.tomykaira.uchronie.git.Commit
import org.eclipse.jgit.diff.DiffEntry
import com.tomykaira.uchronie.GitRepository

case class CommitDecorator(commit: Commit) {
  def tableRow: Array[AnyRef] =
    Array(commit.shortId, commit.message.split("\n").head)

  def diff(repository: GitRepository): List[DiffDecorator] = List()

    /*commit match {
    case Commit.Raw(raw) => repository.diff(raw)
    case Commit.Pick(Commit.Raw(raw)) => repository.diff(raw)
    case Commit.Rename(Commit.Raw(raw), _) => Some(repository.diff(raw))
    case _ => None
  }*/

}
