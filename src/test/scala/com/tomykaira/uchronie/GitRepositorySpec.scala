package com.tomykaira.uchronie

import org.scalatest.{BeforeAndAfter, FunSpec}
import org.scalatest.matchers.ShouldMatchers

class GitRepositorySpec extends FunSpec with BeforeAndAfter with ShouldMatchers with GitSpecHelper {
  before {
    initRepo()
  }

  describe("list commits in range") {
    it("should respond 0 commits with the same ID") {
      val commit = firstCommit
      repository.listCommits(commit.getId, commit.getId) should have size 0
    }

    it("should respond 1 commit with two IDs in sequence") {
      val commit1 = firstCommit
      val commit2 = secondCommit
      repository.listCommits(commit1.getId, commit2.getId) should contain (commit2)
    }

    it("should respond 3 commits in reversed order") {
      val commit1 = firstCommit
      val commit2 = secondCommit
      val commit3 = createCommit("foo", "bar", "commit3")
      val commit4 = createCommit("foo", "baz", "commit4")
      repository.listCommits(commit1.getId, commit4.getId) should equal (List(commit4, commit3, commit2))
    }
  }

  describe("abbreviate") {
    it("should abbreviate ObjectID to human readable ID") {
      val commit = firstCommit
      repository.abbreviate(commit.getId).name should have length 7
    }
  }
}
