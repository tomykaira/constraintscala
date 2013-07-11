package com.tomykaira.uchronie

import scala.swing._
import org.eclipse.jgit.lib.ObjectId
import com.tomykaira.constraintscala.{FSM, StaticConstraint}
import org.eclipse.jgit.revwalk.RevCommit
import scala.annotation.tailrec
import javax.swing.border.EmptyBorder
import akka.actor.{Props, ActorSystem}
import com.tomykaira.uchronie.git._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.concurrent.ExecutionContext.Implicits.global
import scala.swing.event.ButtonClicked
import com.tomykaira.uchronie.git.Commit
import scala.Some
import javax.swing.text.DefaultCaret
import com.tomykaira.uchronie.ui.OperationView

object Main extends SimpleSwingApplication {
  val system = ActorSystem("Worker")
  val actor = system.actorOf(Props[Worker])

  sealed trait ProcessingState
  case class Working() extends ProcessingState
  case class Stopped() extends ProcessingState

  def top: Frame = new MainFrame() {
    title = "Uchronie"

    val graphConstraint = new StaticConstraint[ArrangingGraph](new ArrangingGraph(repository, start, end))

    val processingFSM = new FSM[ProcessingState] {
      state = Stopped()
    }

    val onApply = { () =>
      implicit val timeout = Timeout(60 seconds)

      processingFSM.changeStateTo(Working())

      val future = ask(actor, graphConstraint.get).mapTo[ArrangingGraph]
      future.onComplete {
        case_ =>
          processingFSM.changeStateTo(Stopped())
      }
      future.onSuccess {
        case g => graphConstraint.update(g)
      }
      future.onFailure {
        case err => Dialog.showMessage(title = "Error", message = err.getMessage)
      }
    }

    val performingThread = new StaticConstraint[Option[CommitThread]](None)

    performingThread.onChange {
      case None => processingFSM.changeStateTo(Stopped())
      case Some(_) =>
    }

    performingThread.onChange {
      case Some(thread) =>
      case None =>
    }

    def dispatch(op: Operation) {
      graphConstraint.get.transit(op) match {
        case Right(_) => graphConstraint.update(graphConstraint.get) // FIXME: fake update
        case Left(err) =>
          Dialog.showMessage(title = "Error", message = err)
      }
    }

    val commitsTable = new CommitsTable(graphConstraint)
    processingFSM.onChange {
      case Working() => commitsTable.enabled = false
      case Stopped() => commitsTable.enabled = true
    }

    sealed trait EditState
    case class NotEditing() extends EditState
    case class EditCommit(commit: RevCommit) extends EditState
    case class EditDone(commit: RevCommit) extends EditState
    case class Rebasing(range: GraphRange) extends EditState
    case class RebaseFailed(range: GraphRange) extends EditState
    val editFSM = new FSM[EditState] {
      state = NotEditing()
    }
    def openEditWaitingDialog: Dialog.Result.Value =
      Dialog.showOptions(
        title = "Edit commit",
        message = "Edit files with editor and commit everything.\nThe working tree must be clean to proceed.",
        entries = Seq("Done", "Abort"),
        initial = 0
      )
    def openConflictFixWaitingDialog: Dialog.Result.Value =
      Dialog.showOptions(
        title = "Edit commit",
        message = "Files are conflicted while rebasing.  Fix conflicts and commit all.",
        entries = Seq("Done", "Abort"),
        initial = 0
      )

    editFSM.onChange {
      case _: Rebasing => processingFSM.changeStateTo(Working())
      case _ => processingFSM.changeStateTo(Stopped())
    }

    @tailrec
    def pickInteractively(orphans: GraphRange) {
      editFSM.changeStateTo(Rebasing(orphans))
      orphans.graph.applyInteractively(orphans) match {
        case Left(rest) =>
          editFSM.changeState({ case _: Rebasing => RebaseFailed(rest) })
          openConflictFixWaitingDialog match {
            case Dialog.Result.Yes =>
              pickInteractively(rest)
            case _ =>
              orphans.graph.rollback()
              editFSM.changeStateTo(NotEditing())
          }
        case Right(newGraph) =>
          editFSM.changeState { case _: Rebasing => NotEditing() }
          graphConstraint.update(newGraph)
      }
    }

    editFSM.onChange({
      case EditCommit(commit) =>
        val currentGraph = graphConstraint.get
        val orphans = currentGraph.startEdit(commit)
        openEditWaitingDialog match {
          case Dialog.Result.Yes =>
            orphans match {
              case Some(range) => pickInteractively(range)
              case None => throw new RuntimeException("TODO") // TODO
            }
          case _ =>
            currentGraph.rollback()
            editFSM.changeStateTo(NotEditing())
        }
      case _ =>
    })

    commitsTable.state.onChange({
      case commitsTable.Dropped(range, at) =>
        commitsTable.state.changeStateTo(commitsTable.RowsSelected(range))
        dispatch(Operation.MoveOp(range.commits.list, at))
      case _ =>
    })

    val changedFiles = new FileList(commitsTable.state.convert({
      case commitsTable.RowsSelected(range) =>
        new CommitDecorator(range.first).diff(repository).getOrElse(Nil)
      case _ => Nil
    }))
    val comment = new CommentArea(commitsTable.state.convert({
      case commitsTable.RowsSelected(range) =>
        Some(range)
      case _ => None
    }))
    comment.messageFSM.onChange({
      case comment.Committing(range, message) =>
        range.commits.tail match {
          case Nil => dispatch(Operation.RenameOp(range.first, message))
          case _ => dispatch(Operation.SquashOp(range.commits.list, Some(message)))
        }
      case _ =>
    })
    val changes = new TextArea() {
      editable = false
      changedFiles.selectedItem.onChange({
        case Some(AllFiles(diffs)) => text = new DiffListDecorator(diffs).fullDiff(repository)
        case Some(FileDiff(diff)) => text = new DiffDecorator(diff).formatted(repository)
        case None =>
      })
      peer.getCaret.asInstanceOf[DefaultCaret].setUpdatePolicy(DefaultCaret.NEVER_UPDATE)
    }

    def scrollable(c: Component): ScrollPane =
      new ScrollPane(c) {
        border = new EmptyBorder(0,0,0,0)
      }

    val commitsController = new BorderPanel() {
      val buttons = new GridPanel(1, 2) {
        maximumSize = new Dimension(Integer.MAX_VALUE, 50)
        contents += new Button("Squash") {
          reactions += {
            case e: ButtonClicked =>
              commitsTable.state.get match {
                case commitsTable.RowsSelected(range) =>
                  val newMessage = comment.messageFSM.get match {
                    case comment.Editing(_) => Some(comment.text)
                    case _ => None
                  }
                  dispatch(Operation.SquashOp(range.commits.list, newMessage))
                case _ =>
              }
          }
        }
        contents += new Button("Delete") {
          reactions += {
            case e: ButtonClicked =>
              commitsTable.state.get match {
                case commitsTable.RowsSelected(range) =>
                  range.commits.list foreach {c => dispatch(Operation.DeleteOp(c))}
                case _ =>
              }
          }
        }
        contents += new Button("Edit") {
          reactions += {
            case e: ButtonClicked =>
              commitsTable.state.get match {
                case commitsTable.RowsSelected(range) =>
                  range.first match {
                    case Commit.Raw(r) => editFSM.changeState(NotEditing(), EditCommit(r))
                    case _ => sys.error("Unsupported operation")
                  }
                case _ =>
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
      add(new OperationView(processingFSM, graphConstraint, onApply), BorderPanel.Position.North)
      add(gitView, BorderPanel.Position.Center)
    }

    override def closeOperation() {
      repository.resetToOriginalBranch()
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
