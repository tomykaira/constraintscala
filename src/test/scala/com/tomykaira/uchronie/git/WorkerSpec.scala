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
}
