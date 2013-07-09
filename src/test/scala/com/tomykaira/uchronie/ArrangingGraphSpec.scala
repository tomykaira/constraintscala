package com.tomykaira.uchronie

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{FunSpec, BeforeAndAfter}
import org.scalatest.EitherValues._
import org.eclipse.jgit.lib.Constants
import com.tomykaira.uchronie.git.{Operation, Commit}
import scala.language.reflectiveCalls
import scala.collection.JavaConverters.{asScalaIteratorConverter, collectionAsScalaIterableConverter}

class ArrangingGraphSpec extends FunSpec with BeforeAndAfter with ShouldMatchers with GitSpecHelper {
  before {
    initRepo()
  }

  trait GraphUtilities {
    val commits: List[Commit.Raw]
    lazy val graph = new ArrangingGraph(repository, commits.head, commits.last)
    lazy val first = commits(0)
    lazy val second = commits(1)
    lazy val third = commits(2)
    lazy val fourth = commits(3)

    def emptyRange: GraphRange = graph.selectRange(Seq())

    def messages(g: { val commits: List[Commit] }): List[String] = g.commits.map(_.message)

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
        val short = new ArrangingGraph(repository, first, third)
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
        graph.reorder(emptyRange, 0).right.value should equal (graph.currentThread)
      }
    }
    it("should reorder commits to move the last row to the top") {
      new Fixture {
        val range = graph.selectRange(Seq(2))
        val newGraph = graph.reorder(range, 0)
        messages(newGraph.right.value) should equal (List("2nd", "4th", "3rd"))
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
  }

  describe("squash") {
    it("should do nothing if no commit is selected") {
      new Fixture {
        emptyRange.squash(None).right.value should equal (graph.currentThread)
      }
    }
    it("should do nothing if 1 commit is selected") {
      new Fixture {
        graph.selectRange(Seq(2)).squash(None).right.value should equal (graph.currentThread)
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
        emptyRange.delete().right.value should equal (graph.currentThread)
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
        repository.resolve(Constants.HEAD).get should equal (commits(1).getId)
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

  describe("applyCurrentThread") {
    it("should actually apply the result of operations") {
      val commits = (1 to 10).map { i => createCommit(s"$i.txt", i.toString, i.toString)}.reverse.toList
      val graph = new ArrangingGraph(repository, commits.last, commits.head)
      val result = for {
        t <- graph.transit(Operation.RenameOp(commits(5), "New")).right             // 10 9 8 7 6 New 4 3 2
        t <- graph.transit(Operation.MoveOp(t.commits.slice(5,9), 0)).right         // New 4 3 2 10 9 8 7 6
        t <- graph.transit(Operation.DeleteOp(t.commits(2))).right                  // New 4 2 10 9 8 7 6
        t <- graph.transit(Operation.SquashOp(t.commits.slice(1,4), None)).right    // New 4-2-10 9 8 7 6
        r <- graph.applyCurrentThread.right
      } yield r

      val newGraph: ArrangingGraph = result.right.value
      val newCommits = newGraph.currentThread.commits
      assert(newCommits.forall(_.isInstanceOf[Commit.Raw]))

      val revCommits = repository.git.log().addRange(newGraph.start, newGraph.last).call().iterator().asScala.toList
      revCommits.map(_.getFullMessage) should equal (List("New", "10\n\n2\n\n4", "9", "8", "7", "6"))
    }
  }
}
