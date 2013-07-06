package com.tomykaira.uchronie

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{FunSpec, BeforeAndAfter}
import org.scalatest.EitherValues._
import org.eclipse.jgit.lib.{IndexDiff, Constants}
import org.eclipse.jgit.treewalk.FileTreeIterator
import org.eclipse.jgit.revwalk.RevCommit
import com.tomykaira.uchronie.git.Commit

class ArrangingGraphSpec extends FunSpec with BeforeAndAfter with ShouldMatchers with GitSpecHelper {
  before {
    initRepo()
  }

  trait GraphUtilities {
    val commits: List[Commit]
    lazy val graph = repository.listCommits(commits.head, commits.last)
    lazy val first = commits(0)
    lazy val second = commits(1)
    lazy val third = commits(2)
    lazy val fourth = commits(3)

    def emptyRange: GraphRange = graph.selectRange(Seq())
    def messages(g: ArrangingGraph): List[String] = g.commits.map(_.getFullMessage)
    def commitsInRange(seq: Seq[Int]) =
      graph.selectRange(seq).commits
  }

  trait Fixture extends GraphUtilities {
    val commits = List(
      createCommit("A", "1st", "1st"),
      createCommit("B", "2nd", "2nd"),
      createCommit("C", "3rd", "3rd"),
      createCommit("D", "4th", "4th"))
  }

  trait ConflictFixture extends GraphUtilities {
    val commits = List(
      createCommit("A", "1st", "1st"),
      createCommit("A", "2nd", "2nd"),
      createCommit("A", "3rd", "3rd"),
      createCommit("A", "4th", "4th"))
  }

  describe("updateComment") {
    it("should create new ArrangingGraph inheriting current") {
      new Fixture {
        val result = graph.updateComment(third, "New 3rd").right.value
        messages(result) should equal (List("4th", "New 3rd", "2nd"))
      }
    }
    it("should include all when the first is updated") {
      new Fixture {
        val result = graph.updateComment(second, "New").right.value
        messages(result) should equal (List("4th", "3rd", "New"))
      }
    }
    it("should inherit range after updated") {
      new Fixture {
        val short = repository.listCommits(first, third)
        val result = short.updateComment(second, "New").right.value
        messages(result) should equal (List("3rd", "New"))
      }
    }
  }

  describe("selectRange") {
    it("should refer original graph") {
      new Fixture {
        val range = graph.selectRange(Seq())
        range.graph should equal (graph)
      }
    }
    it("should have no commit if range is empty") {
      new Fixture {
        commitsInRange(Seq()) should have length 0
      }
    }
    it("should have 1 commit in range") {
      new Fixture {
        commitsInRange(Seq(0)) should equal (List(commits.last))
      }
    }
    it("should have 2 commits in range") {
      new Fixture {
        commitsInRange(Seq(0,1)) should equal (List(fourth, third))
      }
    }
    it("should have 2 commits not in sequence") {
      new Fixture {
        commitsInRange(Seq(0,2)) should equal (List(fourth, second))
      }
    }
  }

  describe("reorder") {
    it("should return self if range is empty") {
      new Fixture {
        graph.reorder(emptyRange, 0).right.value should equal (graph)
      }
    }
    it("should reorder commits to move the last row to the top") {
      new Fixture {
        val range = graph.selectRange(Seq(2))
        val newGraph = graph.reorder(range, 0)
        messages(newGraph.right.value) should equal (List("2nd", "4th", "3rd"))
      }
    }
    it("should actually modify git tree") {
      new Fixture {
        val range = graph.selectRange(Seq(2))
        val newCommits = graph.reorder(range, 0).right.value.commits
        newCommits(0).getParent(0) should equal (newCommits(1).raw)
      }
    }
    it("should reorder two commits") {
      new Fixture {
        val range = graph.selectRange(Seq(1,2))
        val newGraph = graph.reorder(range, 0)
        messages(newGraph.right.value) should equal (List("3rd", "2nd", "4th"))
      }
    }
    it("should reorder new to old") {
      new Fixture {
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
      new Fixture {
        emptyRange.squash(None).right.value should equal (graph)
      }
    }
    it("should do nothing if 1 commit is selected") {
      new Fixture {
        graph.selectRange(Seq(2)).squash(None).right.value should equal (graph)
      }
    }
    it("should squash 2 commits into one") {
      new Fixture {
        val result = graph.selectRange(Seq(1,2)).squash(None)
        messages(result.right.value) should equal (List("4th", "2nd\n\n3rd"))
      }
    }
    it("should squash 3 commits into one") {
      new Fixture {
        val result = graph.selectRange(Seq(0,1,2)).squash(None)
        messages(result.right.value) should equal (List("2nd\n\n3rd\n\n4th"))
      }
    }
    it("should fail without operation if range is not sequential") {
      new Fixture {
        val result = graph.selectRange(Seq(0,2)).squash(None)
        result should be ('left)
      }
    }
    it("should accept the new commit message") {
      new Fixture {
        val result = graph.selectRange(Seq(1,2)).squash(Some("new message"))
        messages(result.right.value) should equal (List("4th", "new message"))
      }
    }
  }

  describe("delete") {
    it("should do nothing if no commit is selected") {
      new Fixture {
        emptyRange.delete().right.value should equal (graph)
      }
    }
    it("should create new tree without a specified commit") {
      new Fixture {
        val result = graph.selectRange(Seq(1)).delete()
        messages(result.right.value) should equal (List("4th", "2nd"))
      }
    }
  }

  describe("startEdit") {
    it("should checkout the parent of specified commit") {
      new Fixture {
        graph.startEdit(commits(2))
        repository.resolve(Constants.HEAD).get should equal (commits(1).raw)
      }
    }
    it("should have files indexed") {
      new Fixture {
        graph.startEdit(commits(2))
        repository.isClean should be (false)
      }
    }
    it("should return orphan commits") {
      new Fixture {
        val orphans = graph.startEdit(commits(2))
        orphans.commits should equal (List(commits(3)))
      }
    }
  }

  describe("applyInteractively") {
    it("should return new ArrangingGraph if success") {
      new Fixture {
        // Instantiate graph before run
        val range = graph.selectRange(Seq(0))
        repository.resetHard(second)
        val result = graph.applyInteractively(range)
        messages(result.right.value) should equal (List("4th", "2nd"))
      }
    }
    it("should return succeeding commits if failure") {
      new ConflictFixture {
        repository.resetHard(first)
        val result = graph.applyInteractively(graph.selectRange(Seq(0,1)))
        result.left.value.commits should equal (List(commits.last))
      }
    }
    it("should keep repository dirty") {
      new ConflictFixture {
        repository.resetHard(first)
        graph.applyInteractively(graph.selectRange(Seq(0,1)))
        repository.isClean should be (false)
      }
    }
    it("should return given range if already dirty") {
      new Fixture {
        val range = graph.selectRange(Seq(0))
        repository.resetSoft(first)
        val result = graph.applyInteractively(range)
        result.left.value should equal (range)
      }
    }
  }
}
