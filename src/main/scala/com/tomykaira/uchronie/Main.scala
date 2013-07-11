package com.tomykaira.uchronie

import scala.swing._
import org.eclipse.jgit.lib.ObjectId
import com.tomykaira.constraintscala.FSM
import akka.actor.{Props, ActorSystem}
import com.tomykaira.uchronie.git._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.concurrent.ExecutionContext.Implicits.global
import com.tomykaira.uchronie.ui._
import com.tomykaira.uchronie.ui.SwingHelper._

object Main extends SimpleSwingApplication {
  val system = ActorSystem("Worker")
  val actor = system.actorOf(Props[Worker])

  sealed trait ProcessingState
  case class Working() extends ProcessingState
  case class Stopped() extends ProcessingState

  def top: Frame = new MainFrame() {
    title = "Uchronie"

    val fsm = new FSM[GraphState] {
      state = GraphState.Clean(ArrangingGraph.startUp(repository, start, end))
    }

    fsm.onChange {
      case GraphState.Applying(graph) =>
        implicit val timeout = Timeout(60.seconds)

        val future = ask(actor, graph).mapTo[ArrangingGraph]
        future.onSuccess {
          case g: ArrangingGraph => fsm.changeStateTo(GraphState(g))
        }
        future.onFailure {
          case err =>
            fsm.changeStateTo(GraphState.Modified(graph))
            Dialog.showMessage(title = "Error", message = err.getMessage)
        }
      case GraphState.Editing(manager) =>
        val result = manager.run
        fsm.changeStateTo(GraphState.Clean(result))
      case _ =>
    }

    def dispatch(op: Operation) {
      fsm changeState {
        case s @ (GraphState.Modified(_) | GraphState.Clean(_)) =>
          GraphState.Modified(s.graph.transit(op))
      }
    }

    val commitsTable = new CommitsTable(fsm, dispatch)

    val comment = new CommentArea(fsm, commitsTable.currentRange, dispatch)

    val commitsView = new BorderPanel {
      add(scrollable(commitsTable), BorderPanel.Position.Center)
      add(new CommitsController(fsm, commitsTable.currentRange, dispatch), BorderPanel.Position.South)
    }

    val gitView = new SplitPane(Orientation.Vertical,
      new SplitPane(Orientation.Horizontal, commitsView, scrollable(comment)) {
        dividerLocation = 200
      }, new ChangesView(fsm, commitsTable.currentRange))

    contents = new BorderPanel() {
      add(new OperationView(fsm), BorderPanel.Position.North)
      add(gitView, BorderPanel.Position.Center)
    }

    override def closeOperation() {
      repository.resetToOriginalBranch(fsm.get.graph.last)
      super.closeOperation()
    }
  }

  var repository: GitRepository = _
  var start: ObjectId = _
  var end: ObjectId = _

  override def main(args: Array[String]) {
    new ArgumentParser(args).parse match {
      case Left(e) => initializationError(e)
      case Right(parsed) =>
        repository = new GitRepository(parsed.repository)
        if (!repository.isClean)
          initializationError("Repository is not clean.  Commit everything before start uchronie for safety.")
        repository.resolve(parsed.start) match {
          case Some(id) => start = id
          case None => initializationError("Start SHA-1 " + parsed.start + " is not resolved to one object id")
        }
        repository.resolve(parsed.end) match {
          case Some(id) => end = id
          case None => initializationError("End SHA-1 " + parsed.end + " is not resolved to one object id")
        }
    }
    super.main(args)
  }

  def initializationError(message: String) {
    System.err.println(message)
    System.exit(1)
  }
}
