package com.tomykaira.uchronie

import scala.swing.TextArea
import org.eclipse.jgit.revwalk.RevCommit
import com.tomykaira.constraintscala.{Transition, FSM, Constraint}
import scala.swing.event.{ValueChanged, Key, KeyReleased}
import java.awt.event.InputEvent

class CommentArea(constraint: Constraint[Option[RevCommit]]) extends TextArea {
  sealed trait EditState
  case class NothingSelected() extends EditState
  case class Selected(commit: RevCommit) extends EditState
  case class Editing(commit: RevCommit) extends EditState
  case class Committing(commit: RevCommit, newComment: String) extends EditState
  case class Committed() extends EditState

  listenTo(keys)

  val editFSM = new FSM[EditState] {
    val transitions = List()
    state = NothingSelected()

    def startCommit() {
      changeState({ case Editing(commit) => Committing(commit, text) })
    }
  }

  constraint.onChange({
    case Some(commit) => editFSM.changeStateTo(Selected(commit))
    case None => editFSM.changeStateTo(NothingSelected())
  })

  editFSM.onChange({
    case NothingSelected() | Committing(_,_) =>
      editable = false
      background = java.awt.Color.gray
    case Selected(commit) =>
      text = commit.getFullMessage
      editable = true
      background = java.awt.Color.white
    case Editing(_)  =>
      editable = true
      background = java.awt.Color.white
    case Committed() =>
      editable = true
      background = new java.awt.Color(200, 255, 200)
  })

  reactions += {
    case e: ValueChanged =>
      editFSM.changeState({ case Selected(commit) => Editing(commit) })
    case e: KeyReleased if (e.modifiers & InputEvent.CTRL_DOWN_MASK) == InputEvent.CTRL_DOWN_MASK && e.key == Key.Enter =>
      editFSM.startCommit()
  }
}
