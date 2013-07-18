package io.github.tomykaira.uchronie.ui

import scala.swing.{ListView, Component, Label, Alignment}
import java.awt.Color
import scala.swing.ListView.Renderer
import io.github.tomykaira.constraintscala.Constraint
import scala.swing.event.SelectionChanged
import javax.swing.JList

class DiffList(changesConstraint: Constraint[List[DiffDecorator]]) extends ListView[DiffDecorator] {
  lazy val typedPeer: JList[DiffDecorator] = peer.asInstanceOf[JList[DiffDecorator]]
  val selectedItem = new Constraint[Option[DiffDecorator]]({
    val value = typedPeer.getSelectedValue
    if (value == null) None else Some(value)
  })

  changesConstraint onChange { decorators =>
    listData = decorators
    repaint()
    if (decorators.length > 0)
      selectIndices(0)
  }

  listenTo(selection)

  reactions += {
    case e: SelectionChanged => selectedItem.invalidate()
  }

  renderer = new Renderer[DiffDecorator] {
    val highlightBackground = new Color(192, 192, 255)
    def componentFor(list: ListView[_], isSelected: Boolean, focused: Boolean, entry: DiffDecorator, index: Int): Component = {
      val highlight = isSelected || focused
      new Label {
        text = entry.name
        horizontalAlignment = Alignment.Left
        opaque = true
        background = if(highlight) highlightBackground else java.awt.Color.white
      }
    }
  }
}
