package com.tomykaira.uchronie

import java.io.{PrintWriter, File}
import org.eclipse.jgit.api.Git
import com.tomykaira.uchronie.git.Commit
import org.eclipse.jgit.revwalk.RevCommit

trait GitSpecHelper {
  case class GitSpecHelperException(message: String) extends RuntimeException

  var repoRoot: File = _
  var dotGit: File = _
  var repository: GitRepository = _

  // Reference: https://github.com/kevinsawicki/gitective/blob/master/org.gitective.core/src/test/java/org/gitective/tests/GitTestCase.java
  def initRepo() {
    val tmpDir = System.getProperty("java.io.tmpdir")
    if (tmpDir == null)
      throw new GitSpecHelperException("java.io.tmpdir was null")
    val dir = new File(tmpDir, "git-test-case-" + System.nanoTime())
    if (! dir.mkdir())
      throw new GitSpecHelperException("Failed to create temporary git directory: " + dir.toPath)
    repoRoot = dir
    repoRoot.deleteOnExit()

    Git.init().setDirectory(dir).setBare(false).call()
    dotGit = new File(dir, org.eclipse.jgit.lib.Constants.DOT_GIT)
    if (!dotGit.exists())
      throw new GitSpecHelperException("Failed to initialize git repository under " + dir.toPath)
    git.commit.setMessage("Initialize repository").call()
    repository = new GitRepository(dotGit)
    repository.startWork()
  }

  def git: Git = Git.open(dotGit)

  def createCommit(path: String, content: String, message: String): Commit.Raw = {
    addFile(path, content)
    doCommit(message)
  }

  def doCommit(message: String): Commit.Raw = {
    val commit = git.commit().setAll(true).setMessage(message).call()
    if (commit == null)
      throw new GitSpecHelperException("commit is null")
    Commit.Raw(commit)
  }

  def addFile(path: String, content: String) {
    val file = new File(repoRoot, path)
    if (!file.getParentFile.exists())
      if (!file.getParentFile.mkdirs())
        throw new GitSpecHelperException("Failed to create directories for file " + file.toPath)
    if (!file.exists())
      if (!file.createNewFile())
        throw new GitSpecHelperException("Failed to create file " + file.toPath)
    val writer = new PrintWriter(file)
    try {
      writer.print(content)
    } finally {
      writer.close()
    }

    git.add().addFilepattern(path).call()
  }

  def createCommit(message: String): Commit.Raw =
    createCommit("testFile", message, message)

  def firstCommit =
    createCommit("test", "Hello World", "First Commit")

  def secondCommit =
    createCommit("test2", "Hello World", "Second Commit")

  def dummyCommit(id: Int): Commit.Raw = {
    val raw = s"""tree 9788669ad918b6fcce64af8882fc9a81cb6aba67
author A U. Thor <a_u_thor@example.com> 1218123387 +0700
committer C O. Miter <c@example.com> 1218123390 -0500

Dummy $id"""
    Commit.Raw(RevCommit.parse(raw.getBytes("UTF-8")))
  }
}
