package com.tomykaira.uchronie

import scala.swing.TextArea
import com.tomykaira.constraintscala.{FSM, Constraint}
import scala.swing.event.{Key, KeyReleased}
import java.awt.event.InputEvent
import javax.swing.text.DefaultCaret
import com.tomykaira.uchronie.git.ArrangingGraph

class CommentArea(constraint: Constraint[Option[(ArrangingGraph, TargetRange)]]) extends TextArea {
  sealed trait MessageState
  case class NothingSelected() extends MessageState
  case class Selected(range: TargetRange, defaultMessage: String) extends MessageState
  case class Editing(range: TargetRange) extends MessageState
  case class Committing(range: TargetRange, newComment: String) extends MessageState

  listenTo(keys)

  tooltip = "Edit and Ctrl+Enter to update the commit message"
  peer.getCaret.asInstanceOf[DefaultCaret].setUpdatePolicy(DefaultCaret.UPDATE_WHEN_ON_EDT)

  val messageFSM = new FSM[MessageState] {
    state = NothingSelected()
  }

  constraint.onChange({
    case Some((graph, range)) =>
      messageFSM.changeStateTo(Selected(range, graph.squashMessage(range.list)))
    case None => messageFSM.changeStateTo(NothingSelected())
  })

  messageFSM.onChange({
    case NothingSelected() | Committing(_,_) =>
      editable = false
      background = java.awt.Color.gray
    case Selected(range, message) =>
      text = message
      editable = true
      background = java.awt.Color.white
    case Editing(_)  =>
      editable = true
      background = java.awt.Color.white
  })

  reactions += {
    case e: KeyReleased =>
      if((e.modifiers & InputEvent.CTRL_DOWN_MASK) == InputEvent.CTRL_DOWN_MASK && e.key == Key.Enter)
        messageFSM.changeState({ case Editing(r) => Committing(r, text) })
      else
        messageFSM.changeState({ case Selected(r, _) => Editing(r) })
  }
}
