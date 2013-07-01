package com.tomykaira.uchronie

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{FunSpec, BeforeAndAfter}
import org.scalatest.EitherValues._

class ArrangingGraphSpec extends FunSpec with BeforeAndAfter with ShouldMatchers with GitSpecHelper {
  before {
    initRepo()
  }

  describe("updateComment") {
    it("should create new ArrangingGraph inheriting current") {
      val start = createCommit("1st")
      createCommit("2nd")
      val target = createCommit("3rd")
      val end = createCommit("4th")
      val original = repository.listCommits(start, end)
      original.commits should have length 3 // gauntlet
      val graph = original.updateComment(target, "New 3rd").right.value
      graph.commits.map(_.getFullMessage) should equal (List("4th", "New 3rd", "2nd"))
    }

    it("should include all when the first is updated") {
      val start = createCommit("1st")
      val target = createCommit("2nd")
      createCommit("3rd")
      val end = createCommit("4th")
      val original = repository.listCommits(start, end)
      val graph = original.updateComment(target, "New")
      graph.right.value.commits.map(_.getFullMessage) should equal (List("4th", "3rd", "New"))
    }

    it("should inherit range after updated") {
      val start = createCommit("1st")
      val target = createCommit("2nd")
      val end = createCommit("3rd")
      createCommit("4th")
      val original = repository.listCommits(start, end)
      val graph = original.updateComment(target, "New")
      graph.right.value.commits.map(_.getFullMessage) should equal (List("3rd", "New"))
    }
  }

  trait RangeFixture {
    val commits = List(
      createCommit("A", "1st", "1st"),
      createCommit("B", "2nd", "2nd"),
      createCommit("C", "3rd", "3rd"),
      createCommit("D", "4th", "4th"))
    val graph = repository.listCommits(commits.head, commits.last)
  }

  describe("selectRange") {
    it("should refer original graph") {
      new RangeFixture {
        val range = graph.selectRange(Seq())
        range.graph should equal (graph)
      }
    }
    it("should have no commit if range is empty") {
      new RangeFixture {
        val range = graph.selectRange(Seq())
        range.commits should have length 0
      }
    }
    it("should have 1 commit in range") {
      new RangeFixture {
        val range = graph.selectRange(Seq(0))
        range.commits should equal (List(commits.last))
      }
    }
    it("should have 2 commits in range") {
      new RangeFixture {
        val range = graph.selectRange(Seq(0,1))
        range.commits should equal (List(commits(3), commits(2)))
      }
    }
    it("should have 2 commits not in sequence") {
      new RangeFixture {
        val range = graph.selectRange(Seq(0,2))
        range.commits should equal (List(commits(3), commits(1)))
      }
    }
  }

  describe("reorder") {
    it("should return self if range is empty") {
      new RangeFixture {
        val range = graph.selectRange(Seq())
        graph.reorder(range, 0).right.value should equal (graph)
      }
    }
    it("should reorder commits to move the last row to the top") {
      new RangeFixture {
        val range = graph.selectRange(Seq(2))
        val newGraph = graph.reorder(range, 0)
        newGraph.right.value.commits.map(_.getFullMessage) should equal (List("2nd", "4th", "3rd"))
      }
    }
    it("should actually modify git tree") {
      new RangeFixture {
        val range = graph.selectRange(Seq(2))
        val newCommits = graph.reorder(range, 0).right.value.commits
        newCommits(0).getParent(0) should equal (newCommits(1))
      }
    }
  }
}
