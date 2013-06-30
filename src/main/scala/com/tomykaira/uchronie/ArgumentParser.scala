package com.tomykaira.uchronie

import java.io.File

class ArgumentParser(args: Array[String]) {
  def parse: Either[String, Arguments] = {
    if (args.length != 3)
      return Left("Usage: REPO_ROOT START END")

    val repo = new File(args(0), org.eclipse.jgit.lib.Constants.DOT_GIT)
    if (!repo.isDirectory)
      return Left(".git file " + repo.toPath + " should be a directory")

    Right(Arguments(repo, args(1), args(2)))
  }
}

case class Arguments(repository: File, start: String, end: String)
