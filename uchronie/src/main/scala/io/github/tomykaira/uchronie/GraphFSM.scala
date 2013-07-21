package io.github.tomykaira.uchronie

import io.github.tomykaira.uchronie.ui.GraphState
import io.github.tomykaira.constraintscala.FSM
import akka.actor.{Props, ActorSystem}
import akka.util.Timeout
import scala.concurrent.duration._
import akka.pattern.ask
import scala.concurrent.ExecutionContext.Implicits.global
import io.github.tomykaira.uchronie.git.{Operation, ArrangingGraph, Worker}
import scala.swing.Dialog
import org.eclipse.jgit.lib.ObjectId

class GraphFSM(repository: GitRepository, start: ObjectId, last: ObjectId) extends FSM[GraphState] {
  state = GraphState.Clean(ArrangingGraph.startUp(repository, start, last))

  private[this] val system = ActorSystem("Worker")
  private[this] val actor = system.actorOf(Props[Worker])
  private[this] implicit val timeout = Timeout(60.seconds)

  onChange {
    case GraphState.Applying(graph) =>
      val future = ask(actor, graph).mapTo[ArrangingGraph]
      future.onSuccess {
        case g: ArrangingGraph => changeStateTo(GraphState(g))
      }
      future.onFailure {
        case err =>
          changeStateTo(GraphState.Modified(graph))
          Dialog.showMessage(title = "Error", message = err.getMessage)
      }
    case GraphState.Editing(manager) =>
      ask(actor, manager).mapTo[ArrangingGraph] onSuccess {
        case result: ArrangingGraph.Clean =>
          changeStateTo(GraphState.Clean(result))
      }
    case _ =>
  }

  val dispatch: Operation => Unit = { op =>
    changeState {
      case s @ (GraphState.Modified(_) | GraphState.Clean(_)) =>
        GraphState.Modified(s.graph.transit(op))
    }
  }
}
