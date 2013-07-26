package io.github.tomykaira.uchronie.git

import io.github.tomykaira.uchronie.GitSpecHelper
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{FunSpec, BeforeAndAfter}
import akka.actor.{Props, ActorSystem}
import org.scalatest.concurrent.AsyncAssertions.Waiter

class WorkerSpec extends FunSpec with BeforeAndAfter with ShouldMatchers with GitSpecHelper {
  before {
    initRepo()
  }

  trait Utilities {
    val commits: List[Commit.Raw]
    lazy val graph = ArrangingGraph.startUp(repository, commits.head, commits.last)

    def messages(g: ArrangingGraph): List[String] = g.commits.map(_.message)

    val system = ActorSystem("test")
    val actor = system.actorOf(Props[Worker])
    val w = new Waiter
  }

  trait Fixture extends Utilities {
    val commits = List(
      createCommit("A", "1st", "1st"),
      createCommit("A", "2nd", "2nd"),
      createCommit("A", "3rd", "3rd"),
      createCommit("A", "4th", "4th"))
  }

  describe("on receiving ArrangingGraph") {
    it("should apply the operations") {
      new Fixture {
        val modified = graph.transit(Operation.RenameOp(1, "New 3rd"))
        actor ! Request(modified, { g =>
          w { messages(g) should equal (List("4th", "New 3rd", "2nd")) }
          w.dismiss()
        }, { _ => })
        w.await()
      }
    }
    it("report CherryPickFailed on failure") {
      new Fixture {
        val modified = graph.transit(Operation.DeleteOp(1))
        actor ! Request(modified, { _ => }, { err =>
          w { err.toString should include ("Cherry pick failed") }
          w.dismiss()
        })
        w.await()
      }
    }
    it("should return itself if clean") {
      new Fixture {
        actor ! Request(graph, { g =>
          w { g should equal (graph) }
          w.dismiss()
        }, { _ => })
        w.await()
      }
    }
  }
}
