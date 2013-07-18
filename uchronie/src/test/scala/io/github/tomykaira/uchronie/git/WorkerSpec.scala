package io.github.tomykaira.uchronie.git

import io.github.tomykaira.uchronie.GitSpecHelper
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{FunSpec, BeforeAndAfter}
import akka.actor.{Props, ActorSystem}
import org.scalatest.concurrent.AsyncAssertions.Waiter
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

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
    def dispatch(g: ArrangingGraph): Future[ArrangingGraph] =
      ask(actor, g).mapTo[ArrangingGraph]

    implicit val timeout = Timeout(5.seconds)
  }

  trait Fixture extends Utilities {
    val commits = List(
      createCommit("A", "1st", "1st"),
      createCommit("B", "2nd", "2nd"),
      createCommit("C", "3rd", "3rd"),
      createCommit("D", "4th", "4th"))
  }

  describe("on receiving ArrangingGraph") {
    it("should apply the operations") {
      new Fixture {
        val modified = graph.transit(Operation.RenameOp(1, "New 3rd"))
        dispatch(modified).onSuccess {
          case newGraph =>
            w { messages(newGraph) should equal (List("4th", "New 3rd", "2nd")) }
            w.dismiss()
        }
        w.await()
      }
    }
    it("should return itself if clean") {
      new Fixture {
        dispatch(graph).onSuccess {
          case newGraph =>
            w { newGraph should equal (graph) }
            w.dismiss()
        }
        w.await()
      }
    }
  }
}
