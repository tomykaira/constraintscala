package com.tomykaira.uchronie

import org.scalatest.{BeforeAndAfter, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import org.eclipse.jgit.revwalk.RevCommit
import java.nio.file.Files
import scala.sys.process._

class GitRepositorySpec extends FunSpec with BeforeAndAfter with ShouldMatchers with GitSpecHelper {
  before {
    initRepo()
  }

  describe("list commits in range") {
    it("should respond 0 commits with the same ID") {
      val commit: RevCommit = firstCommit
      repository.listCommits(commit.getId, commit.getId) should have size 0
    }

    it("should respond 1 commit with two IDs in sequence") {
      val commit1: RevCommit = firstCommit
      val commit2: RevCommit = secondCommit
      repository.listCommits(commit1.getId, commit2.getId) should contain (commit2)
    }

    it("should respond 3 commits in reversed order") {
      val commit1: RevCommit = firstCommit
      val commit2: RevCommit = secondCommit
      val commit3: RevCommit = createCommit("foo", "bar", "commit3")
      val commit4: RevCommit = createCommit("foo", "baz", "commit4")
      repository.listCommits(commit1.getId, commit4.getId) should equal (List(commit4, commit3, commit2))
    }
  }

  describe("toCommit") {
    it("should convert ObjectId to RevCommit") {
      val commit: RevCommit = firstCommit
      repository.toCommit(commit.getId) should equal (commit)
    }
  }

  describe("resolve") {
    it("should expand abbreviated ObjectId to the internal expression") {
      val commit = firstCommit
      val id = repository.resolve(commit.abbreviate(7).name())
      id.get should equal (commit.getId)
    }

    it("should return None for empty string") {
      repository.resolve("") should equal (None)
    }

    it("should return None if commit not found") {
      repository.resolve("0123456") should equal (None)
    }

    // This test creates many commits.  Slow.
    it("should return None if commit is ambiguous") {
      def short(commit: RevCommit) = commit.getId.name.substring(0,2)
      val commit = firstCommit
      var similarCommit = createCommit("file", "xxxxxx", "trying")

      while (short(commit) != short(similarCommit)) {
        similarCommit = createCommit("file", "At " + System.nanoTime().toString, "trying")
      }
      val id = repository.resolve(short(commit))
      id should equal (None)
    }
  }

  describe("diff") {
    it("should list changed files") {
      createCommit("test", "first", "First commit")
      val commit = createCommit("test", "second", "second commit")
      val diff = repository.diff(commit)
      diff should have length 1
      diff(0).getNewPath should equal ("test")
    }
  }

  describe("resetToOriginalBranch") {
    it("should checkout master") {
      repository.resetToOriginalBranch(repository.head)
      repository.repository.getBranch should equal ("master")
    }
    it("should back to specified commit") {
      val first = firstCommit
      secondCommit
      repository.resetToOriginalBranch(first)
      repository.head should equal (first.id)
    }
    it("should delete working branch") {
      val last = secondCommit
      val workBranch = repository.repository.getBranch
      repository.resetToOriginalBranch(last)
      repository.resolve(workBranch) should be (None)
    }
    it("should not reset if work is not started") {
      val first = firstCommit
      val repository = new GitRepository(dotGit)
      addFile("foo", "bar")
      repository.resetToOriginalBranch(first)
      assert(!repository.isClean)
    }
  }

  describe("isClean") {
    it("should handle buggy symlink as clean") {
      createCommit("a", "A's content", "Create A")
      Files.createSymbolicLink(repoRoot.toPath.resolve("b"), repoRoot.toPath.resolve("a"))
      Process("git add b", Some(repoRoot)).!
      Process(Seq("git", "commit", "-m", "Create B"), Some(repoRoot)).!
      assert(repository.isClean)
    }
  }
}
