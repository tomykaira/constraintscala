package com.tomykaira.uchronie.git

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{FunSpec, BeforeAndAfter}
import org.scalatest.EitherValues._
import scala.collection.JavaConverters.asScalaIteratorConverter
import com.tomykaira.uchronie.{TargetRange, GitSpecHelper}
import scala.language.reflectiveCalls

class ArrangingGraphSpec extends FunSpec with BeforeAndAfter with ShouldMatchers with GitSpecHelper {
  before {
    initRepo()
  }

  trait GraphUtilities {
    val commits: List[Commit.Raw]
    lazy val graph = ArrangingGraph.startUp(repository, commits.head, commits.last)
    lazy val first = commits(0)
    lazy val second = commits(1)
    lazy val third = commits(2)
    lazy val fourth = commits(3)

    def messages(g: { val commits: List[Commit] }): List[String] = g.commits.map(_.message)

    def commitsInRange(h: Int, t: Int*) =
      graph.rowsToCommits(h :: t.toList)
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

  describe("transit") {
    it("should rename commits in currentThread") {
      new Fixture {
        val next = graph.transit(Operation.RenameOp(1, "New 3rd"))
        messages(next) should equal (List("4th", "New 3rd", "2nd"))
      }
    }
    it("should move commits") {
      new Fixture {
        val next = graph.transit(Operation.MoveOp(TargetRange(1, 2), 0))
        messages(next) should equal (List("3rd", "2nd", "4th"))
      }
    }
    it("should squash 2 commits") {
      new Fixture {
        val next = graph.transit(Operation.SquashOp(TargetRange(1, 2), None))
        messages(next) should equal (List("4th", "2nd\n\n3rd"))
      }
    }
    it("should delete the specified commit") {
      new Fixture {
        val next = graph.transit(Operation.DeleteOp(1))
        messages(next) should equal (List("4th", "2nd"))
      }
    }
  }

  describe("applyCurrentThread") {
    it("should return no range object if all commits are deleted") {
      val commits = List(firstCommit, secondCommit).reverse
      val before = ArrangingGraph.startUp(repository, commits.last, commits.head)
      before.commits should have length 1
      val after = before.transit(Operation.DeleteOp(0)).applyCurrentThread.right.value
      after.start should equal (after.last)
      after.commits should be ('empty)
    }
    it("should actually apply the result of operations") {
      val commits = (1 to 10).map { i => createCommit(s"$i.txt", i.toString, i.toString)}.reverse.toList
      val result = ArrangingGraph.startUp(repository, commits.last, commits.head).
        transit(Operation.RenameOp(5, "New")).                   // 10 9 8 7 6 New 4 3 2
        transit(Operation.MoveOp(TargetRange(5, 8), 0)).         // New 4 3 2 10 9 8 7 6
        transit(Operation.DeleteOp(2)).                          // New 4 2 10 9 8 7 6
        transit(Operation.SquashOp(TargetRange(1, 3), None)).    // New 4-2-10 9 8 7 6
        applyCurrentThread

      val newGraph: ArrangingGraph = result.right.value
      val newCommits = newGraph.commits
      assert(newCommits.forall(_.isInstanceOf[Commit.Raw]))

      val revCommits = repository.git.log().addRange(newGraph.start, newGraph.last).call().iterator().asScala.toList
      revCommits.map(_.getFullMessage) should equal (List("New", "10\n\n2\n\n4", "9", "8", "7", "6"))
    }
  }

  describe("startEdit") {
    it("should reset to specified commit") {
      val commits = (1 to 5).map { i => createCommit(s"$i.txt", i.toString, i.toString)}.reverse.toList
      val graph = ArrangingGraph.startUp(repository, commits.last, commits.head)
      val result = graph.startEdit(2)    // 5 4 _3_ 2 (1)
      repository.head should equal (commits(3).raw)
      result.commits.head should equal (commits(1))
    }
  }
}
