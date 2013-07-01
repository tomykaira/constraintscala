package com.tomykaira.uchronie

import scala.swing.{ListView, Component, Label, Alignment}
import org.eclipse.jgit.diff.DiffEntry
import java.awt.Color
import scala.swing.ListView.Renderer
import com.tomykaira.constraintscala.Constraint

class FileList(changesConstraint: Constraint[List[DiffEntry]]) extends ListView[DiffEntry] {
  changesConstraint.onChange({data => listData = data; repaint() })

  listenTo(selection)

  renderer = new Renderer[DiffEntry] {
    val highlightBackground = new Color(192, 192, 255)
    def componentFor(list: ListView[_], isSelected: Boolean, focused: Boolean, diff: DiffEntry, index: Int): Component = {
      val highlight = isSelected || focused
      new Label {
        text = diff.getChangeType match {
          case DiffEntry.ChangeType.ADD | DiffEntry.ChangeType.MODIFY => diff.getNewPath
          case DiffEntry.ChangeType.DELETE => diff.getOldPath
          case DiffEntry.ChangeType.COPY | DiffEntry.ChangeType.RENAME =>
            diff.getOldPath + " -> " + diff.getNewPath
        }
        horizontalAlignment = Alignment.Left
        opaque = true
        background = if(highlight) highlightBackground else java.awt.Color.white
      }
    }
  }
}
