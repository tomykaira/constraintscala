package io.github.tomykaira.uchronie.ui

import io.github.tomykaira.uchronie.TargetRange
import io.github.tomykaira.uchronie.git.{EditManager, ArrangingGraph}
import scala.swing.Dialog

class DialogEditManager(graph: ArrangingGraph.Clean, range: TargetRange) extends EditManager(graph, range) {
  def onEditWaiting: UserResponse = {
    toResponse(Dialog.showOptions(
      title = "Edit commit",
      message = "Edit files with editor and commit everything.\nThe working tree must be clean to proceed.",
      entries = Seq("Done", "Abort"),
      initial = 0
    ))
  }

  def onConflictFixWaiting: UserResponse = {
    toResponse(Dialog.showOptions(
      title = "Edit commit",
      message = "Files are conflicted while rebasing.  Fix conflicts and commit all.",
      entries = Seq("Done", "Abort"),
      initial = 0
    ))
  }

  private[this] def toResponse(result: Dialog.Result.Value): UserResponse = result match {
    case Dialog.Result.Yes => Continue
    case Dialog.Result.No => Abort
  }
}
