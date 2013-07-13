package com.tomykaira.uchronie.ui

import scala.swing.{Swing, Table}
import javax.swing.table.DefaultTableModel
import com.tomykaira.constraintscala.{Constraint, FSM}
import javax.swing._
import java.awt.datatransfer.{Transferable, DataFlavor}
import javax.activation.{DataHandler, ActivationDataFlavor}
import scala.swing.event.TableRowsSelected
import com.tomykaira.uchronie.{GraphFSM, TargetRange}
import com.tomykaira.uchronie.git.Operation


class CommitsTable(fsm: GraphFSM) extends Table {
  sealed trait OperationState
  case class NoOperation() extends OperationState
  case class RowsSelected(range: TargetRange) extends OperationState
  case class Dragging(range: TargetRange) extends OperationState
  case class Dropped(range: TargetRange, at: TargetRange.Index) extends OperationState

  fsm onChange {
    case GraphState.Clean(_) | GraphState.Modified(_) => enabled = true
    case _ => enabled = false
  }

  override lazy val model = super.model.asInstanceOf[DefaultTableModel]
  autoResizeMode = Table.AutoResizeMode.LastColumn

  listenTo(selection)

  model addColumn "SHA1"
  model addColumn "Comment"

  peer.getColumnModel.getColumn(0).setMaxWidth(100)

  peer.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION)

  override protected def editor(row: Int, column: Int) = null

  private[this] val state = new FSM[OperationState] {
    state = NoOperation()
  }

  state.onChange({
    case Dropped(range, at) =>
      state.changeStateTo(RowsSelected(range))
      fsm.dispatch(Operation.MoveOp(range, at))
    case _ =>
  })

  val currentRange: Constraint[Option[TargetRange]] = state.convert {
    case RowsSelected(range) => Some(range)
    case _ => None
  }

  private def selectedRow: Option[Int] = {
    val row = peer.getSelectedRow
    if (row == -1) None else Some(row)
  }

  reactions += {
    case _: TableRowsSelected => {
      peer.getSelectedRows.toList match {
        case Nil => state.changeStateTo(NoOperation())
        case list => state.changeStateTo(RowsSelected(TargetRange(list)))
      }
    }
  }

  fsm onChange { state =>
    Swing.onEDT {
      val oldSelected = selectedRow
      for (i <- 0 to model.getRowCount - 1) {
        model.removeRow(0)
      }
      state.graph.commits.foreach { commit =>
        model.addRow(new CommitDecorator(commit).tableRow)
      }
      oldSelected.foreach(row =>
        if (row < peer.getRowCount)
          peer.setRowSelectionInterval(row, row))
    }
  }

  // Drag & drop set of commits
  peer.getSelectionModel.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION)
  peer.setTransferHandler(new CommitTransferHandler)
  peer.setDropMode(DropMode.INSERT_ROWS)
  peer.setDragEnabled(true)

  class CommitTransferHandler extends TransferHandler {
    private[this] val flavor = new ActivationDataFlavor(classOf[Object], DataFlavor.javaJVMLocalObjectMimeType, "Part of ArrangingGraph object")

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
