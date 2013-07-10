package com.tomykaira.uchronie.ui

import org.eclipse.jgit.diff.DiffEntry
import com.tomykaira.uchronie.GitRepository

sealed trait DiffDecorator {
  val name: String

  def fullDiff(repository: GitRepository): String
}

object DiffDecorator {
  case class Each(diff: DiffEntry) extends DiffDecorator {
    val name = diff.getChangeType match {
      case DiffEntry.ChangeType.ADD | DiffEntry.ChangeType.MODIFY => diff.getNewPath
      case DiffEntry.ChangeType.DELETE => diff.getOldPath
      case DiffEntry.ChangeType.COPY | DiffEntry.ChangeType.RENAME =>
        diff.getOldPath + " -> " + diff.getNewPath
    }

    def fullDiff(repository: GitRepository): String = {
      repository.formatDiff(diff)
    }
  }

  case class All(prefix: String, diffs: List[DiffEntry]) extends DiffDecorator {
    val name = s"ALL of $prefix"

    def fullDiff(repository: GitRepository): String = {
      diffs.map(DiffDecorator.Each(_).fullDiff(repository)).mkString("\n\n")
    }
  }
}