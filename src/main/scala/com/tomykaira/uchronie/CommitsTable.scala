package com.tomykaira.uchronie

import scala.swing.{Dialog, Table}
import javax.swing.table.DefaultTableModel
import com.tomykaira.constraintscala.{StaticConstraint, Constraint}
import org.eclipse.jgit.revwalk.RevCommit
import javax.swing._
import java.awt.datatransfer.{Transferable, DataFlavor}
import javax.activation.{DataHandler, ActivationDataFlavor}
import scala.Some
import scala.swing.event.TableRowsSelected

class CommitsTable(graphConstraint: StaticConstraint[ArrangingGraph]) extends Table {
  override lazy val model = super.model.asInstanceOf[DefaultTableModel]
  autoResizeMode = Table.AutoResizeMode.LastColumn

  listenTo(selection)

  model addColumn "SHA1"
  model addColumn "Comment"

  peer.getColumnModel.getColumn(0).setMaxWidth(100)
  peer.getSelectionModel.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION)
  peer.setTransferHandler(new CommitTransferHandler)
  peer.setDropMode(DropMode.INSERT_ROWS)
  peer.setDragEnabled(true)

  def selectedRow: Option[Int] = {
    val row = peer.getSelectedRow
    if (row == -1) None else Some(row)
  }

  val selectedCommit = new Constraint[Option[RevCommit]]({
    selectedRow.flatMap(row => graphConstraint.get(row))
  })
  val selectedRange = new Constraint[GraphRange]({
    graphConstraint.get.selectRange(peer.getSelectedRows)
  })

  reactions += {
    case e: TableRowsSelected if !e.adjusting => {
      selectedCommit.invalidate()
      selectedRange.invalidate()
    }
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

  class CommitTransferHandler extends TransferHandler {
    private val flavor = new ActivationDataFlavor(classOf[GraphRange], DataFlavor.javaJVMLocalObjectMimeType, "Part of ArrangingGraph object")

    override def createTransferable(c: JComponent): Transferable = {
      val table = c.asInstanceOf[JTable]
      new DataHandler(selectedRange.get, flavor.getMimeType)
    }

    override def canImport(support: TransferHandler.TransferSupport): Boolean = {
      support.isDrop &&
        support.isDataFlavorSupported(flavor) &&
        graphConstraint.get.contains(graphRange(support))
    }

    override def getSourceActions(c: JComponent): Int = TransferHandler.MOVE

    override def importData(support: TransferHandler.TransferSupport): Boolean = {
      if (!canImport(support)) return false
      val dl = support.getDropLocation.asInstanceOf[JTable.DropLocation]
      graphConstraint.get.reorder(graphRange(support), dl.getRow) match {
        case Left(err) =>
          Dialog.showMessage(title = "Error", message = err)
          false
        case Right(next) =>
          graphConstraint.update(next)
          true
      }
    }

    private def graphRange(support: TransferHandler.TransferSupport): GraphRange =
      support.getTransferable.getTransferData(flavor).asInstanceOf[GraphRange]
  }
}
