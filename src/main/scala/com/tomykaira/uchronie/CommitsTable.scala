package com.tomykaira.uchronie

import scala.swing.{Dialog, Table}
import javax.swing.table.DefaultTableModel
import com.tomykaira.constraintscala.{Transition, FSM, StaticConstraint, Constraint}
import org.eclipse.jgit.revwalk.RevCommit
import javax.swing._
import java.awt.datatransfer.{Transferable, DataFlavor}
import javax.activation.{DataHandler, ActivationDataFlavor}
import scala.Some
import scala.swing.event.TableRowsSelected

class CommitsTable(graph: StaticConstraint[ArrangingGraph]) extends Table {
  sealed trait OperationState
  case class NoOperation() extends OperationState
  case class RowsSelected(range: GraphRange) extends OperationState
  case class Dragging(range: GraphRange) extends OperationState
  case class Dropped(range: GraphRange, at: Int) extends OperationState

  override lazy val model = super.model.asInstanceOf[DefaultTableModel]
  autoResizeMode = Table.AutoResizeMode.LastColumn

  listenTo(selection)

  model addColumn "SHA1"
  model addColumn "Comment"

  peer.getColumnModel.getColumn(0).setMaxWidth(100)

  val state = new FSM[OperationState] {
    state = NoOperation()
  }
  def selectedRow: Option[Int] = {
    val row = peer.getSelectedRow
    if (row == -1) None else Some(row)
  }

  reactions += {
    case _: TableRowsSelected => {
      val range = peer.getSelectedRows
      if (range.isEmpty) {
        state.changeStateTo(NoOperation())
      } else {
        state.changeStateTo(RowsSelected(graph.get.selectRange(range)))
      }
    }
  }

  graph.onChange({ arrangingGraph =>
    val oldSelected = selectedRow
    for (i <- 0 to model.getRowCount - 1) {
      model.removeRow(0)
    }
    arrangingGraph.commits.foreach(commit =>
      model addRow new CommitDecorator(commit).tableRow(arrangingGraph.repository))
    oldSelected.foreach(row =>
      if (row < peer.getRowCount)
        peer.setRowSelectionInterval(row, row))
  })

  // Drag & drop set of commits
  peer.getSelectionModel.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION)
  peer.setTransferHandler(new CommitTransferHandler)
  peer.setDropMode(DropMode.INSERT_ROWS)
  peer.setDragEnabled(true)

  class CommitTransferHandler extends TransferHandler {
    private val flavor = new ActivationDataFlavor(classOf[Object], DataFlavor.javaJVMLocalObjectMimeType, "Part of ArrangingGraph object")

    override def createTransferable(c: JComponent): Transferable = {
      state.changeState({ case RowsSelected(range) => Dragging(range) })
      new DataHandler(state, flavor.getMimeType)
    }

    override def canImport(support: TransferHandler.TransferSupport): Boolean = {
      support.isDrop &&
        state.get.isInstanceOf[Dragging]
    }

    override def getSourceActions(c: JComponent): Int = TransferHandler.MOVE

    override def importData(support: TransferHandler.TransferSupport): Boolean = {
      if (!canImport(support)) return false
      val dl = support.getDropLocation.asInstanceOf[JTable.DropLocation]
      state.changeState({ case Dragging(range) => Dropped(range, dl.getRow) })

      // dropping action always fails.
      // update of constraint handles result
      false
    }
  }
}
