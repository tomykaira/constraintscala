package com.tomykaira.uchronie

import org.scalatest.{BeforeAndAfter, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import org.eclipse.jgit.revwalk.RevCommit
import org.eclipse.jgit.lib.Constants

class GitRepositorySpec extends FunSpec with BeforeAndAfter with ShouldMatchers with GitSpecHelper {
  before {
    initRepo()
  }

  describe("list commits in range") {
    it("should respond 0 commits with the same ID") {
      val commit = firstCommit
      repository.listCommits(commit.getId, commit.getId).commits should have size 0
    }

    it("should respond 1 commit with two IDs in sequence") {
      val commit1 = firstCommit
      val commit2 = secondCommit
      repository.listCommits(commit1.getId, commit2.getId).commits should contain (commit2)
    }

    it("should respond 3 commits in reversed order") {
      val commit1 = firstCommit
      val commit2 = secondCommit
      val commit3 = createCommit("foo", "bar", "commit3")
      val commit4 = createCommit("foo", "baz", "commit4")
      repository.listCommits(commit1.getId, commit4.getId).commits should equal (List(commit4, commit3, commit2))
    }
  }

  describe("toCommit") {
    it("should convert ObjectId to RevCommit") {
      val commit = firstCommit
      repository.toCommit(commit.getId) should equal (commit)
    }
  }

  describe("abbreviate") {
    it("should abbreviate ObjectID to human readable ID") {
      val commit = firstCommit
      repository.abbreviate(commit.getId).name should have length 7
    }
  }

  describe("resolve") {
    it("should expand abbreviated ObjectId to the internal expression") {
      val commit = firstCommit
      val shortSha1 = repository.abbreviate(commit.getId).name
      val id = repository.resolve(shortSha1)
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
      repository.resetToOriginalBranch()
      repository.repository.getBranch should equal ("master")
    }
    it("should not change HEAD") {
      secondCommit
      val beforeHead = repository.resolve(Constants.HEAD)
      repository.resetToOriginalBranch()
      val afterHead = repository.resolve(Constants.HEAD)
      afterHead should equal (beforeHead)
    }
    it("should delete working branch") {
      secondCommit
      val workBranch = repository.repository.getBranch
      repository.resetToOriginalBranch()
      val afterHead = repository.resolve(workBranch)
      afterHead should be (None)
    }
  }
}
