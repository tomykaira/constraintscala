package com.tomykaira.uchronie

import scala.swing.TextArea
import org.eclipse.jgit.revwalk.RevCommit
import com.tomykaira.constraintscala.Constraint
import scala.swing.event.{Key, KeyReleased}
import java.awt.event.InputEvent

class CommentArea(constraint: Constraint[Option[RevCommit]]) extends TextArea {
  listenTo(keys)
  editable = true

  constraint.onChange(commit => text = commit.map(_.getFullMessage).getOrElse(""))

  reactions += {
    case e: KeyReleased if (e.modifiers & InputEvent.CTRL_DOWN_MASK) == InputEvent.CTRL_DOWN_MASK && e.key == Key.Enter =>
      println("Ctrl + Enter detected")
  }
}
