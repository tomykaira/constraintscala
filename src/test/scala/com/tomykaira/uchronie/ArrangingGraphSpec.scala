package com.tomykaira.uchronie

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{FunSpec, BeforeAndAfter}
import org.scalatest.EitherValues._
import org.eclipse.jgit.lib.Constants
import com.tomykaira.uchronie.git.{Operation, Commit}
import scala.language.reflectiveCalls
import scala.collection.JavaConverters.asScalaIteratorConverter
import scalaz.NonEmptyList

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

    def messages(g: { val commits: List[Commit] }): List[String] = g.commits.map(_.message)

    def commitsInRange(h: Int, t: Int*) =
      graph.rowsToCommits(NonEmptyList.nel(h, t.toList)).list
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
        graph.transit(Operation.RenameOp(1, "New 3rd"))
        messages(graph.currentThread) should equal (List("4th", "New 3rd", "2nd"))
      }
    }
    it("should move commits") {
      new Fixture {
        graph.transit(Operation.MoveOp(TargetRange(1, 2), 0))
        messages(graph.currentThread) should equal (List("3rd", "2nd", "4th"))
      }
    }
    it("should squash 2 commits") {
      new Fixture {
        graph.transit(Operation.SquashOp(TargetRange(1, 2), None))
        messages(graph.currentThread) should equal (List("4th", "2nd\n\n3rd"))
      }
    }
    it("should delete the specified commit") {
      new Fixture {
        graph.transit(Operation.DeleteOp(1))
        messages(graph.currentThread) should equal (List("4th", "2nd"))
      }
    }
  }

  describe("applyCurrentThread") {
    it("should actually apply the result of operations") {
      val commits = (1 to 10).map { i => createCommit(s"$i.txt", i.toString, i.toString)}.reverse.toList
      val graph = new ArrangingGraph(repository, commits.last, commits.head)
      val result = for {
        t <- graph.transit(Operation.RenameOp(5, "New")).right             // 10 9 8 7 6 New 4 3 2
        t <- graph.transit(Operation.MoveOp(TargetRange(5, 8), 0)).right         // New 4 3 2 10 9 8 7 6
        t <- graph.transit(Operation.DeleteOp(2)).right                  // New 4 2 10 9 8 7 6
        t <- graph.transit(Operation.SquashOp(TargetRange(1, 3), None)).right    // New 4-2-10 9 8 7 6
        r <- graph.applyCurrentThread.right
      } yield r

      val newGraph: ArrangingGraph = result.right.value
      val newCommits = newGraph.currentThread.commits
      assert(newCommits.forall(_.isInstanceOf[Commit.Raw]))

      val revCommits = repository.git.log().addRange(newGraph.start, newGraph.last).call().iterator().asScala.toList
      revCommits.map(_.getFullMessage) should equal (List("New", "10\n\n2\n\n4", "9", "8", "7", "6"))
    }
  }

  describe("startEdit") {
    it("should reset to specified commit") {
      val commits = (1 to 5).map { i => createCommit(s"$i.txt", i.toString, i.toString)}.reverse.toList
      val graph = new ArrangingGraph(repository, commits.last, commits.head)
      val result = graph.startEdit(2)    // 5 4 _3_ 2 (1)
      repository.head should equal (commits(3).raw)
      result.commits.head should equal (commits(1))
    }
  }
}
