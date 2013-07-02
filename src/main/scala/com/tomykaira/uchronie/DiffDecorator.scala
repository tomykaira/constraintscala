package com.tomykaira.uchronie

import org.eclipse.jgit.diff.DiffEntry

class DiffDecorator(diff: DiffEntry) {
  def path: String = diff.getChangeType match {
    case DiffEntry.ChangeType.ADD | DiffEntry.ChangeType.MODIFY => diff.getNewPath
    case DiffEntry.ChangeType.DELETE => diff.getOldPath
    case DiffEntry.ChangeType.COPY | DiffEntry.ChangeType.RENAME =>
      diff.getOldPath + " -> " + diff.getNewPath
  }

  def formatted(repository: GitRepository): String = {
    repository.formatDiff(diff)
  }
}
