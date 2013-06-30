package com.tomykaira.uchronie

import org.scalatest.{BeforeAndAfter, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import org.eclipse.jgit.revwalk.RevCommit
import org.scalatest.EitherValues._

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

  describe("update commit comment") {
    val comment = "Hello New World"

    it("should change comment of existing commit") {
      val commit = firstCommit
      val newCommit = repository.updateComment(commit, comment)
      newCommit.right.value.getFullMessage should equal (comment)
    }

    it("should return the rebased last commit") {
      val secondComment = "second"
      val first  = firstCommit
      createCommit("file", "content", secondComment)
      val newCommit = repository.updateComment(first, comment)
      newCommit.right.value.getFullMessage should equal (secondComment)
    }
  }
}
