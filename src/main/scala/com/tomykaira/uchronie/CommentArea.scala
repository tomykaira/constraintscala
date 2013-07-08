package com.tomykaira.uchronie

import scala.swing.TextArea
import com.tomykaira.constraintscala.{FSM, Constraint}
import scala.swing.event.{Key, KeyReleased}
import java.awt.event.InputEvent
import javax.swing.text.DefaultCaret

class CommentArea(constraint: Constraint[Option[GraphRange]]) extends TextArea {
  sealed trait MessageState
  case class NothingSelected() extends MessageState
  case class Selected(commit: GraphRange) extends MessageState
  case class Editing(commit: GraphRange) extends MessageState
  case class Committing(commit: GraphRange, newComment: String) extends MessageState

  listenTo(keys)

  tooltip = "Edit and Ctrl+Enter to update the commit message"
  peer.getCaret.asInstanceOf[DefaultCaret].setUpdatePolicy(DefaultCaret.UPDATE_WHEN_ON_EDT)

  val messageFSM = new FSM[MessageState] {
    state = NothingSelected()
  }

  constraint.onChange({
    case Some(range) if range.commits.length > 0 =>
      messageFSM.changeStateTo(Selected(range))
    case None => messageFSM.changeStateTo(NothingSelected())
  })

  messageFSM.onChange({
    case NothingSelected() | Committing(_,_) =>
      editable = false
      background = java.awt.Color.gray
    case Selected(range) =>
      text = range.squashMessage
      editable = true
      background = java.awt.Color.white
    case Editing(_)  =>
      editable = true
      background = java.awt.Color.white
  })

  reactions += {
    case e: KeyReleased =>
      if((e.modifiers & InputEvent.CTRL_DOWN_MASK) == InputEvent.CTRL_DOWN_MASK && e.key == Key.Enter)
        messageFSM.changeState({ case Editing(commit) => Committing(commit, text) })
      else
        messageFSM.changeState({ case Selected(commit) => Editing(commit) })
  }
}
