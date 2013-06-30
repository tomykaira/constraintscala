package com.tomykaira.uchronie

import org.eclipse.jgit.revwalk.RevCommit

case class CommitDecorator(revCommit: RevCommit) {
  def tableRow(repository: GitRepository): Array[String] =
    Array(repository.abbreviate(revCommit.getId).name, revCommit.getShortMessage)
}
