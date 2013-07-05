package com.tomykaira.uchronie

import org.eclipse.jgit.revwalk.RevCommit

case class CommitDecorator(revCommit: RevCommit) {
  def tableRow(repository: GitRepository): Array[AnyRef] =
    Array(repository.abbreviate(revCommit.getId).name, revCommit.getShortMessage)

  def fullDiff(repository: GitRepository): String = {
    val diffs = repository.diff(revCommit)
    diffs.map(new DiffDecorator(_).formatted(repository)).mkString("\n\n")
  }
}
