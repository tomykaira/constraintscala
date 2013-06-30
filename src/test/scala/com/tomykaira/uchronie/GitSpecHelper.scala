package com.tomykaira.uchronie

import java.io.{PrintWriter, File}
import org.eclipse.jgit.api.Git
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
    repository = new GitRepository(dotGit)
  }

  def createCommit(path: String, content: String, message: String): RevCommit = {
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

    val git = Git.open(dotGit)
    git.add().addFilepattern(path).call()
    val commit = git.commit().setOnly(path).setMessage(message).call()
    if (commit == null)
      throw new GitSpecHelperException("commit is null")
    commit
  }

  def firstCommit =
    createCommit("test", "Hello World", "First Commit")

  def secondCommit =
    createCommit("test2", "Hello World", "Second Commit")
}
