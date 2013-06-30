package com.tomykaira.uchronie

import org.eclipse.jgit.revwalk.RevCommit

case class CommitDecorator(revCommit: RevCommit) {
  def tableRow(repository: GitRepository): Array[AnyRef] =
    Array(repository.abbreviate(revCommit.getId).name, revCommit.getShortMessage)
}
