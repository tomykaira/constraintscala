package com.tomykaira.uchronie

import org.eclipse.jgit.diff.DiffEntry

class DiffListDecorator(diffs: List[DiffEntry]) {
  def fullDiff(repository: GitRepository): String = {
    diffs.map(new DiffDecorator(_).formatted(repository)).mkString("\n\n")
  }
}
