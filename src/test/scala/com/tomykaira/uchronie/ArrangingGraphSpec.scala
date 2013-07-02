package com.tomykaira.uchronie

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{FunSpec, BeforeAndAfter}
import org.scalatest.EitherValues._
import org.eclipse.jgit.lib.{IndexDiff, Constants}
import org.eclipse.jgit.treewalk.FileTreeIterator

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

    def emptyRange: GraphRange = graph.selectRange(Seq())
    def messages(graph: ArrangingGraph): List[String] = graph.commits.map(_.getFullMessage)
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
        graph.reorder(emptyRange, 0).right.value should equal (graph)
      }
    }
    it("should reorder commits to move the last row to the top") {
      new RangeFixture {
        val range = graph.selectRange(Seq(2))
        val newGraph = graph.reorder(range, 0)
        messages(newGraph.right.value) should equal (List("2nd", "4th", "3rd"))
      }
    }
    it("should actually modify git tree") {
      new RangeFixture {
        val range = graph.selectRange(Seq(2))
        val newCommits = graph.reorder(range, 0).right.value.commits
        newCommits(0).getParent(0) should equal (newCommits(1))
      }
    }
    it("should reorder two commits") {
      new RangeFixture {
        val range = graph.selectRange(Seq(1,2))
        val newGraph = graph.reorder(range, 0)
        messages(newGraph.right.value) should equal (List("3rd", "2nd", "4th"))
      }
    }
    it("should reorder new to old") {
      new RangeFixture {
        val range = graph.selectRange(Seq(0))
        val newGraph = graph.reorder(range, 3)
        messages(newGraph.right.value) should equal (List("3rd", "2nd", "4th"))
      }
    }
    it("should reset to the original branch if failed") {
      val commits = List(
        createCommit("A", "1st", "1st"),
        createCommit("A", "2st", "2st"),
        createCommit("A", "3st", "3st")
      )
      val graph = repository.listCommits(commits.head, commits.last)
      val range = graph.selectRange(Seq(0))
      val result = graph.reorder(range, 2)
      result should be ('left)
      repository.resolve(Constants.HEAD).get should equal (commits.last.getId)
    }
  }

  describe("squash") {
    it("should do nothing if no commit is selected") {
      new RangeFixture {
        emptyRange.squash().right.value should equal (graph)
      }
    }
    it("should do nothing if 1 commit is selected") {
      new RangeFixture {
        graph.selectRange(Seq(2)).squash().right.value should equal (graph)
      }
    }
    it("should squash 2 commits into one") {
      new RangeFixture {
        val result = graph.selectRange(Seq(1,2)).squash()
        messages(result.right.value) should equal (List("4th", "2nd\n\n3rd"))
      }
    }
    it("should squash 3 commits into one") {
      new RangeFixture {
        val result = graph.selectRange(Seq(0,1,2)).squash()
        messages(result.right.value) should equal (List("2nd\n\n3rd\n\n4th"))
      }
    }
    it("should fail without operation if range is not sequential") {
      new RangeFixture {
        val result = graph.selectRange(Seq(0,2)).squash()
        result should be ('left)
      }
    }
  }

  describe("delete") {
    it("should do nothing if no commit is selected") {
      new RangeFixture {
        emptyRange.delete().right.value should equal (graph)
      }
    }
    it("should create new tree without a specified commit") {
      new RangeFixture {
        val result = graph.selectRange(Seq(1)).delete()
        messages(result.right.value) should equal (List("4th", "2nd"))
      }
    }
  }

  trait EditFixture {
    val commits = List(
      createCommit("A", "1st", "1st"),
      createCommit("B", "2nd", "2nd"),
      createCommit("C", "3rd", "3rd"),
      createCommit("D", "4th", "4th"))
    val graph = repository.listCommits(commits.head, commits.last)
  }

  describe("startEdit") {
    it("should checkout the parent of specified commit") {
      new EditFixture {
        graph.startEdit(commits(2))
        repository.resolve(Constants.HEAD).get should equal (commits(1))
      }
    }
    it("should have files indexed") {
      new EditFixture {
        graph.startEdit(commits(2))
        val diff = new IndexDiff(repository.repository, Constants.HEAD, new FileTreeIterator(repository.repository))
        diff.diff() should be (true)
      }
    }
    it("should return orphan commits") {
      new EditFixture {
        val orphans = graph.startEdit(commits(2))
        orphans.commits should equal (List(commits(3)))
      }
    }
  }
}
