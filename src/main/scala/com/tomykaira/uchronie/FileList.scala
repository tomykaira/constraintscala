package com.tomykaira.uchronie

import scala.swing.{ListView, Component, Label, Alignment}
import org.eclipse.jgit.diff.DiffEntry
import java.awt.Color
import scala.swing.ListView.Renderer
import com.tomykaira.constraintscala.Constraint
import scala.swing.event.SelectionChanged
import javax.swing.JList

sealed trait FileListEntry
case class AllFiles(diffs: List[DiffEntry]) extends FileListEntry
case class FileDiff(diff: DiffEntry) extends FileListEntry

class FileList(changesConstraint: Constraint[List[DiffEntry]]) extends ListView[FileListEntry] {
  lazy val typedPeer: JList[FileListEntry] = peer.asInstanceOf[JList[FileListEntry]]
  val selectedItem = new Constraint[Option[FileListEntry]]({
    val value = typedPeer.getSelectedValue
    if (value == null) None else Some(value)
  })

  changesConstraint.onChange({data =>
    listData = AllFiles(data) :: data.map(FileDiff(_))
    repaint()
    if (data.length > 0)
      selectIndices(0)
  })

  listenTo(selection)

  reactions += {
    case e: SelectionChanged => selectedItem.invalidate()
  }

  renderer = new Renderer[FileListEntry] {
    val highlightBackground = new Color(192, 192, 255)
    def componentFor(list: ListView[_], isSelected: Boolean, focused: Boolean, entry: FileListEntry, index: Int): Component = {
      val highlight = isSelected || focused
      new Label {
        text = entry match {
          case AllFiles(_) => "All"
          case FileDiff(diff) => new DiffDecorator(diff).path
        }
        horizontalAlignment = Alignment.Left
        opaque = true
        background = if(highlight) highlightBackground else java.awt.Color.white
      }
    }
  }
}
