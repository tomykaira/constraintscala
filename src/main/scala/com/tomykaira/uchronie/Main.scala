package com.tomykaira.uchronie

import scala.swing._
import org.eclipse.jgit.lib.ObjectId
import com.tomykaira.constraintscala.FSM
import javax.swing.border.EmptyBorder
import akka.actor.{Props, ActorSystem}
import com.tomykaira.uchronie.git._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.concurrent.ExecutionContext.Implicits.global
import javax.swing.text.DefaultCaret
import com.tomykaira.uchronie.ui._
import scala.swing.event.ButtonClicked
import com.tomykaira.uchronie.ui.CommitDecorator

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

    val commitsTable = new CommitsTable(fsm)

    commitsTable.state.onChange({
      case commitsTable.Dropped(range, at) =>
        commitsTable.state.changeStateTo(commitsTable.RowsSelected(range))
        dispatch(Operation.MoveOp(range, at))
      case _ =>
    })

    val changedFiles = new DiffList(commitsTable.state.convert({
      case commitsTable.RowsSelected(range) =>
        fsm.get.graph.rowsToCommits(range.list) flatMap {c => new CommitDecorator(c).diff(repository) }
      case _ => Nil
    }))
    val comment = new CommentArea(commitsTable.state.convert({
      case commitsTable.RowsSelected(range) =>
        Some((fsm.get.graph, range))
      case _ => None
    }))
    comment.messageFSM.onChange({
      case comment.Committing(range, message) =>
        if (range.isSingleton)
          dispatch(Operation.RenameOp(range.start, message))
        else
          dispatch(Operation.SquashOp(range, Some(message)))
      case _ =>
    })
    val changes = new TextArea() {
      editable = false
      changedFiles.selectedItem.onChange({
        case Some(decorator) => text = decorator.fullDiff(repository)
        case None =>
      })
      peer.getCaret.asInstanceOf[DefaultCaret].setUpdatePolicy(DefaultCaret.NEVER_UPDATE)
    }

    def scrollable(c: Component): ScrollPane =
      new ScrollPane(c) {
        border = new EmptyBorder(0,0,0,0)
      }

    def currentRange: Option[TargetRange] = commitsTable.state.get match {
        case commitsTable.RowsSelected(range) => Some(range)
        case _ => None
      }

    val commitsController = new BorderPanel() {
      val buttons = new GridPanel(1, 2) {
        maximumSize = new Dimension(Integer.MAX_VALUE, 50)
        contents += new Button("Squash") {
          reactions += {
            case e: ButtonClicked =>
              currentRange.foreach { range =>
                val newMessage = comment.messageFSM.get match {
                  case comment.Editing(_) => Some(comment.text)
                  case _ => None
                }
                dispatch(Operation.SquashOp(range, newMessage))
              }
          }
        }
        contents += new Button("Delete") {
          reactions += {
            case e: ButtonClicked => currentRange.foreach { range =>
              range.list foreach {c => dispatch(Operation.DeleteOp(c))}
            }
          }
        }
        contents += new Button("Edit") {
          tooltip = "Edit is available when you have no pending operations"
          fsm.onChange {
            case GraphState.Clean(_) => enabled = true
            case _ => enabled = false
          }
          reactions += {
            case e: ButtonClicked => currentRange.foreach { range =>
              fsm changeState {
                case GraphState.Clean(g) => GraphState.Editing(new EditManager(g, range))
              }
            }
          }
        }
      }
      add(scrollable(commitsTable), BorderPanel.Position.Center)
      add(buttons, BorderPanel.Position.South)
    }

    val gitView = new SplitPane(Orientation.Vertical,
      new SplitPane(Orientation.Horizontal, commitsController, scrollable(comment)) {
        dividerLocation = 200
      },
      new SplitPane(Orientation.Horizontal, scrollable(changedFiles), scrollable(changes)) {
        dividerLocation = 200
      })

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
