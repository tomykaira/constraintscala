package com.tomykaira.uchronie

import scala.swing.{ListView, Component, Label, Alignment}
import org.eclipse.jgit.diff.DiffEntry
import java.awt.Color
import scala.swing.ListView.Renderer
import com.tomykaira.constraintscala.Constraint
import scala.swing.event.SelectionChanged
import javax.swing.JList

class FileList(changesConstraint: Constraint[List[DiffEntry]]) extends ListView[DiffEntry] {
  lazy val typedPeer: JList[DiffEntry] = peer.asInstanceOf[JList[DiffEntry]]
  val selectedItem = new Constraint[Option[DiffEntry]]({
    val value = typedPeer.getSelectedValue
    if (value == null) None else Some(value)
  })

  changesConstraint.onChange({data =>
    listData = data
    repaint()
    if (data.length > 0)
      selectIndices(0)
  })

  listenTo(selection)

  reactions += {
    case e: SelectionChanged => selectedItem.invalidate()
  }

  renderer = new Renderer[DiffEntry] {
    val highlightBackground = new Color(192, 192, 255)
    def componentFor(list: ListView[_], isSelected: Boolean, focused: Boolean, diff: DiffEntry, index: Int): Component = {
      val highlight = isSelected || focused
      new Label {
        text = new DiffDecorator(diff).path
        horizontalAlignment = Alignment.Left
        opaque = true
        background = if(highlight) highlightBackground else java.awt.Color.white
      }
    }
  }
}
