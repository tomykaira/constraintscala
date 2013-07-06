package com.tomykaira.uchronie.git

import com.tomykaira.uchronie.{ArrangingGraph, GraphRange, GitSpecHelper}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{FunSpec, BeforeAndAfter}
import org.eclipse.jgit.revwalk.RevCommit
import akka.actor.{Props, ActorSystem}
import org.scalatest.concurrent.AsyncAssertions.Waiter
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class WorkerSpec extends FunSpec with BeforeAndAfter with ShouldMatchers with GitSpecHelper {
  before {
    initRepo()
  }

  trait Utilities {
    val commits: List[RevCommit]
    lazy val graph = repository.listCommits(commits.head, commits.last)
    lazy val first = commits(0)
    lazy val second = commits(1)
    lazy val third = commits(2)
    lazy val fourth = commits(3)

    def emptyRange: GraphRange = graph.selectRange(Seq())
    def messages(g: ArrangingGraph): List[String] = g.commits.map(_.getFullMessage)
    def commitsInRange(seq: Seq[Int]) =
      graph.selectRange(seq).commits

    val system = ActorSystem("test")
    val actor = system.actorOf(Props[Worker])
    val w = new Waiter
    def dispatch(c: Command): Future[ArrangingGraph] =
      ask(actor, c).mapTo[ArrangingGraph]

    implicit val timeout = Timeout(5 seconds)
  }

  trait Fixture extends Utilities {
    val commits = List(
      createCommit("A", "1st", "1st"),
      createCommit("B", "2nd", "2nd"),
      createCommit("C", "3rd", "3rd"),
      createCommit("D", "4th", "4th"))
  }

  trait ConflictFixture extends Utilities {
    val commits = List(
      createCommit("A", "1st", "1st"),
      createCommit("A", "2nd", "2nd"),
      createCommit("A", "3rd", "3rd"),
      createCommit("A", "4th", "4th"))
  }

  describe("UpdateComment command") {
    it("should update commit comment") {
      new Fixture {
        dispatch(UpdateComment(graph, third, "New 3rd")).onSuccess {
          case newGraph =>
            w { messages(newGraph) should equal (List("4th", "New 3rd", "2nd")) }
            w.dismiss()
        }
        w.await()
      }
    }
  }

  describe("Reorder command") {
    it("should reorder commits to move the last row to the top") {
      new Fixture {
        val range = graph.selectRange(Seq(2))
        dispatch(Reorder(graph, range, 0)).onSuccess {
          case newGraph =>
            w { messages(newGraph) should equal (List("2nd", "4th", "3rd")) }
            w.dismiss()
        }
        w.await()
      }
    }
    it("should report failure if cherry-pick failed") {
      new ConflictFixture {
        val range = graph.selectRange(Seq(2))
        dispatch(Reorder(graph, range, 0)).onFailure {
          case error =>
            w { error.getMessage should include ("Cherry-pick failed") }
            w.dismiss()
        }
        w.await()
      }
    }
  }

  describe("Squash command") {
    it("should squash 2 commits into one") {
      new Fixture {
        dispatch(Squash(graph, graph.selectRange(Seq(1,2)), None)).onSuccess {
          case newGraph =>
            w { messages(newGraph) should equal (List("4th", "2nd\n\n3rd")) }
            w.dismiss()
        }
        w.await()
      }
    }
  }

  describe("Delete command") {
    it("should delete a commit") {
      new Fixture {
        dispatch(Delete(graph, graph.selectRange(Seq(1)))).onSuccess {
          case newGraph =>
            w { messages(newGraph) should equal (List("4th", "2nd")) }
            w.dismiss()
        }
        w.await()
      }
    }
  }
}
