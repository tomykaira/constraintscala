package com.tomykaira.uchronie.ui

import org.eclipse.jgit.diff.DiffEntry
import com.tomykaira.uchronie.GitRepository

class DiffDecorator(diff: DiffEntry) {
  def path: String = diff.getChangeType match {
    case DiffEntry.ChangeType.ADD | DiffEntry.ChangeType.MODIFY => diff.getNewPath
    case DiffEntry.ChangeType.DELETE => diff.getOldPath
    case DiffEntry.ChangeType.COPY | DiffEntry.ChangeType.RENAME =>
      diff.getOldPath + " -> " + diff.getNewPath
  }

  def fullDiff(repository: GitRepository): String = {
    repository.formatDiff(diff)
  }

  def name: String = path
}

class DiffListDecorator(diffs: List[DiffEntry]) {
  def fullDiff(repository: GitRepository): String = {
    diffs.map(new DiffDecorator(_).fullDiff(repository)).mkString("\n\n")
  }
}
