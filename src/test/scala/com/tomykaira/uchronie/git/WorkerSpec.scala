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
    val commits: List[Commit.Raw]
    lazy val graph = new ArrangingGraph(repository, commits.head, commits.last)

    def messages(g: ArrangingGraph): List[String] = g.commits.map(_.getFullMessage)

    val system = ActorSystem("test")
    val actor = system.actorOf(Props[Worker])
    val w = new Waiter
    def dispatch(g: ArrangingGraph): Future[ArrangingGraph] =
      ask(actor, g).mapTo[ArrangingGraph]

    implicit val timeout = Timeout(5 seconds)
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
        graph.transit(Operation.RenameOp(commits(2), "New 3rd"))
        dispatch(graph).onSuccess {
          case newGraph =>
            w { messages(newGraph) should equal (List("4th", "New 3rd", "2nd")) }
            w.dismiss()
        }
        w.await()
      }
    }
  }
}
