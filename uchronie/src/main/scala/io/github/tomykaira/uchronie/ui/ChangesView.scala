package io.github.tomykaira.uchronie.ui

import javax.swing.text.DefaultCaret
import scala.swing.{Orientation, SplitPane, TextArea}
import io.github.tomykaira.constraintscala.{Constraint, FSM}
import io.github.tomykaira.uchronie.{TargetRange, GitRepository}
import io.github.tomykaira.uchronie.ui.SwingHelper._

class ChangesView(fsm: FSM[GraphState], currentRange: Constraint[Option[TargetRange]]) extends SplitPane {
  def repository: GitRepository = fsm.get.graph.repository

  val changedFiles = new DiffList(currentRange.convert({
    case Some(range) =>
      fsm.get.graph.rowsToCommits(range.list) flatMap {c => new CommitDecorator(c).diff(repository) }
    case None => Nil
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
