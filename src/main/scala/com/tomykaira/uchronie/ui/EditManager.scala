package com.tomykaira.uchronie.ui

import com.tomykaira.uchronie.git.{IncrementalEditor, ArrangingGraph}
import com.tomykaira.uchronie.TargetRange
import scala.swing.Dialog
import scala.annotation.tailrec
import com.tomykaira.uchronie.Main.{ProcessingState, Stopped, Working}
import com.tomykaira.constraintscala.{FSM, StaticConstraint}

// TODO: shorten args
class EditManager(graph: ArrangingGraph.Clean, range: TargetRange, graphConstraint: StaticConstraint[ArrangingGraph], processingFSM: FSM[ProcessingState]) {
  def abort() {
    graphConstraint.update(graph.rollback)
  }

  def finish(done: IncrementalEditor.Done) {
    graphConstraint.update(ArrangingGraph.renew(graph, done.head))
  }

  def processing[A](f: => A): A = {
    processingFSM.changeStateTo(Working())
    val result = f
    processingFSM.changeStateTo(Stopped())
    result
  }

  @tailrec
  private def loop(next: IncrementalEditor): Unit = next match {
    case done: IncrementalEditor.Done =>
      finish(done)
    case going: IncrementalEditor.Going =>
      processing { going.continue } match {
        case done: IncrementalEditor.Done => finish(done)
        case rest: IncrementalEditor.Going =>
          openConflictFixWaitingDialog match {
            case Dialog.Result.Yes =>
              loop(rest)
            case Dialog.Result.No =>
              abort()
          }
      }
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

  def run() {
    val going = processing { graph.startEdit(range.end) }

    openEditWaitingDialog match {
      case Dialog.Result.Yes => loop(going)
      case Dialog.Result.No => abort()
    }
  }

}
