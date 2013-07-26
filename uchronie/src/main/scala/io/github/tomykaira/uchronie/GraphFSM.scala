package io.github.tomykaira.uchronie

import io.github.tomykaira.uchronie.ui.GraphState
import io.github.tomykaira.constraintscala.FSM
import akka.actor.{Props, ActorSystem}
import io.github.tomykaira.uchronie.git.{Request, Operation, ArrangingGraph, Worker}
import scala.swing.Dialog
import org.eclipse.jgit.lib.ObjectId

class GraphFSM(repository: GitRepository, start: ObjectId, last: ObjectId) extends FSM[GraphState] {
  state = GraphState.Clean(ArrangingGraph.startUp(repository, start, last))

  private[this] val system = ActorSystem("Worker")
  private[this] val actor = system.actorOf(Props[Worker])

  onChange {
    case GraphState.Applying(graph) =>
      actor ! Request(graph, { g =>
        changeStateTo(GraphState(g))
      }, { err =>
        changeStateTo(GraphState.Modified(graph))
        Dialog.showMessage(title = "Error", message = err.getMessage)
      })
    case GraphState.Editing(manager) =>
      actor ! Request(manager, { g =>
        changeStateTo(GraphState(g))
      }, { err =>
        changeStateTo(GraphState.Clean(manager.graph))
        Dialog.showMessage(title = "Error", message = err.getMessage)
      })
    case _ =>
  }

  val dispatch: Operation => Unit = { op =>
    changeState {
      case s @ (GraphState.Modified(_) | GraphState.Clean(_)) =>
        GraphState.Modified(s.graph.transit(op))
    }
  }
}
