package com.tomykaira.uchronie

import scala.swing.Table
import scala.swing.event.TableRowsSelected
import javax.swing.table.DefaultTableModel
import com.tomykaira.constraintscala.{StaticConstraint, Constraint}
import org.eclipse.jgit.revwalk.RevCommit

class CommitsTable(graphConstraint: StaticConstraint[ArrangingGraph]) extends Table {
  override lazy val model = super.model.asInstanceOf[DefaultTableModel]
  autoResizeMode = Table.AutoResizeMode.LastColumn

  listenTo(selection)

  model addColumn "SHA1"
  model addColumn "Comment"

  peer.getColumnModel.getColumn(0).setMaxWidth(100)

  def selectedRow: Option[Int] = {
    val row = peer.getSelectedRow
    if (row == -1) None else Some(row)
  }

  val selectedCommit = new Constraint[Option[RevCommit]]({
    selectedRow.flatMap(row => graphConstraint.get(row))
  })

  reactions += {
    case e: TableRowsSelected if !e.adjusting => { selectedCommit.invalidate() }
  }

  graphConstraint.onChange({ graph =>
    val oldSelected = selectedRow
    for (i <- 0 to model.getRowCount - 1) {
      model.removeRow(0)
    }
    graph.commits.foreach(commit =>
      model addRow new CommitDecorator(commit).tableRow(graph.repository))
    oldSelected.foreach(row => peer.setRowSelectionInterval(row, row))
  })
}
