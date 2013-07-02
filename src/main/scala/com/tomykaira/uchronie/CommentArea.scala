package com.tomykaira.uchronie

import scala.swing.TextArea
import org.eclipse.jgit.revwalk.RevCommit
import com.tomykaira.constraintscala.{Transition, FSM, Constraint}
import scala.swing.event.{ValueChanged, Key, KeyReleased}
import java.awt.event.InputEvent

class CommentArea(constraint: Constraint[Option[RevCommit]]) extends TextArea {
  sealed trait MessageState
  case class NothingSelected() extends MessageState
  case class Selected(commit: RevCommit) extends MessageState
  case class Editing(commit: RevCommit) extends MessageState
  case class Committing(commit: RevCommit, newComment: String) extends MessageState

  listenTo(keys)

  tooltip = "Edit and Ctrl+Enter to update the commit message"

  val messageFSM = new FSM[MessageState] {
    state = NothingSelected()
  }

  constraint.onChange({
    case Some(commit) => messageFSM.changeStateTo(Selected(commit))
    case None => messageFSM.changeStateTo(NothingSelected())
  })

  messageFSM.onChange({
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
  })

  reactions += {
    case e: ValueChanged =>
      messageFSM.changeState({ case Selected(commit) => Editing(commit) })
    case e: KeyReleased if (e.modifiers & InputEvent.CTRL_DOWN_MASK) == InputEvent.CTRL_DOWN_MASK && e.key == Key.Enter =>
      messageFSM.changeState({ case Editing(commit) => Committing(commit, text) })
  }
}
