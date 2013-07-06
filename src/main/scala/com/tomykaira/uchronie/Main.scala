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
import com.tomykaira.uchronie.git.UpdateComment
import com.tomykaira.uchronie.git.Reorder
import scala.Some

object Main extends SimpleSwingApplication {
  val system = ActorSystem("Worker")
  val actor = system.actorOf(Props[Worker])

  def top: Frame = new MainFrame() {
    title = "Uchronie"

    val graph = repository.listCommits(start, end)
    val graphConstraint = new StaticConstraint[ArrangingGraph](graph)

    def dispatch(c: Command) {
      implicit val timeout = Timeout(60 seconds)
      val future = ask(actor, c).mapTo[ArrangingGraph]
      future.onSuccess {
        case g => graphConstraint.update(g)
      }
      future.onFailure {
        case err => Dialog.showMessage(title = "Error", message = err.getMessage)
      }
    }

    val commitsTable = new CommitsTable(graphConstraint)

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

    @tailrec
    def pickInteractively(orphans: GraphRange) {
      editFSM.changeStateTo(Rebasing(orphans))
      orphans.applyInteractively match {
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
          graphConstraint.update(newGraph)
      }
    }

    editFSM.onChange({
      case EditCommit(commit) =>
        val currentGraph = graphConstraint.get
        val orphans = currentGraph.startEdit(commit)
        openEditWaitingDialog match {
          case Dialog.Result.Yes =>
            pickInteractively(orphans)
          case _ =>
            currentGraph.rollback()
            editFSM.changeStateTo(NotEditing())
        }
      case _ =>
    })

    commitsTable.state.onChange({
      case commitsTable.Dropped(range, at) =>
        commitsTable.state.changeStateTo(commitsTable.RowsSelected(range))
        dispatch(Reorder(range.graph, range, at))
      case _ =>
    })

    val changedFiles = new FileList(commitsTable.state.convert({
      case commitsTable.RowsSelected(range) =>
        range.first.map(c => repository.diff(c)).getOrElse(Nil)
      case _ => Nil
    }))
    val comment = new CommentArea(commitsTable.state.convert({
      case commitsTable.RowsSelected(range) =>
        range.first
      case _ => None
    }))
    comment.messageFSM.onChange({
      case comment.Committing(commit, message) =>
        dispatch(UpdateComment(graphConstraint.get, commit, message))
      case _ =>
    })
    val changes = new TextArea() {
      editable = false
      changedFiles.selectedItem.onChange({
        case Some(AllFiles(diffs)) => text = new DiffListDecorator(diffs).fullDiff(repository)
        case Some(FileDiff(diff)) => text = new DiffDecorator(diff).formatted(repository)
        case None =>
      })
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
                  dispatch(Squash(range.graph, range, newMessage))
                case _ =>
              }
          }
        }
        contents += new Button("Delete") {
          reactions += {
            case e: ButtonClicked =>
              commitsTable.state.get match {
                case commitsTable.RowsSelected(range) =>
                  dispatch(Delete(range.graph, range))
                case _ =>
              }
          }
        }
        contents += new Button("Edit") {
          reactions += {
            case e: ButtonClicked =>
              commitsTable.state.get match {
                case commitsTable.RowsSelected(range) =>
                  range.first.foreach(commit => editFSM.changeState(NotEditing(), EditCommit(commit)))
                case _ =>
              }
          }
        }
      }
      add(new ScrollPane(commitsTable) { border = new EmptyBorder(0,0,0,0) }, BorderPanel.Position.Center)
      add(buttons, BorderPanel.Position.South)
    }

    contents = new SplitPane(Orientation.Vertical,
      new SplitPane(Orientation.Horizontal, commitsController, comment) {
        dividerLocation = 200
      },
      new SplitPane(Orientation.Horizontal, changedFiles, changes) {
        dividerLocation = 200
      })
  }

  var repository: GitRepository = _
  var start: ObjectId = _
  var end: ObjectId = _

  override def main(args: Array[String]) {
    new ArgumentParser(args).parse match {
      case Left(e) => sys.error(e)
      case Right(parsed) =>
        repository = new GitRepository(parsed.repository)
        repository.resolve(parsed.start) match {
          case Some(id) => start = id
          case None => sys.error("Start SHA-1 " + parsed.start + " is not resolved to one object id")
        }
        repository.resolve(parsed.end) match {
          case Some(id) => end = id
          case None => sys.error("End SHA-1 " + parsed.end + " is not resolved to one object id")
        }
    }
    super.main(args)
  }
}
