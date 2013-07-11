package com.tomykaira.uchronie.ui

import javax.swing.text.DefaultCaret
import scala.swing.{Orientation, SplitPane, TextArea}
import com.tomykaira.constraintscala.FSM
import com.tomykaira.uchronie.GitRepository
import com.tomykaira.uchronie.ui.SwingHelper._

class ChangesView(fsm: FSM[GraphState], commitsTable: CommitsTable) extends SplitPane {
  def repository: GitRepository = fsm.get.graph.repository

  val changedFiles = new DiffList(commitsTable.state.convert({
    case commitsTable.RowsSelected(range) =>
      fsm.get.graph.rowsToCommits(range.list) flatMap {c => new CommitDecorator(c).diff(repository) }
    case _ => Nil
  }))

  val changes = new TextArea() {
    editable = false
    changedFiles.selectedItem.onChange({
      case Some(decorator) => text = decorator.fullDiff(repository)
      case None =>
    })
    peer.getCaret.asInstanceOf[DefaultCaret].setUpdatePolicy(DefaultCaret.NEVER_UPDATE)
  }

  orientation = Orientation.Horizontal
  dividerLocation = 200
  leftComponent = scrollable(changedFiles)
  rightComponent = scrollable(changes)
}
