package com.tomykaira.uchronie.ui

import com.tomykaira.uchronie.git.{IncrementalEditor, ArrangingGraph}
import com.tomykaira.uchronie.TargetRange
import scala.swing.Dialog
import scala.annotation.tailrec

class EditManager(val graph: ArrangingGraph.Clean, range: TargetRange) {
  def abort: ArrangingGraph.Clean =
    graph.rollback

  def finish(done: IncrementalEditor.Done): ArrangingGraph.Clean =
    ArrangingGraph.renew(graph, done.head)

  @tailrec
  private def loop(next: IncrementalEditor): ArrangingGraph.Clean = next match {
    case done: IncrementalEditor.Done =>
      finish(done)
    case going: IncrementalEditor.Going =>
      going.continue match {
        case done: IncrementalEditor.Done => finish(done)
        case rest: IncrementalEditor.Going =>
          openConflictFixWaitingDialog match {
            case Dialog.Result.Yes =>
              loop(rest)
            case Dialog.Result.No =>
              abort
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

  def run: ArrangingGraph.Clean = {
    val going = graph.startEdit(range.end)

    openEditWaitingDialog match {
      case Dialog.Result.Yes => loop(going)
      case Dialog.Result.No => abort
    }
  }
}
