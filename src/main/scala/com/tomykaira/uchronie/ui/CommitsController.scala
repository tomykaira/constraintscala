package com.tomykaira.uchronie.ui

import scala.swing.{Button, GridPanel}
import java.awt.Dimension
import scala.swing.event.ButtonClicked
import com.tomykaira.uchronie.git.Operation
import com.tomykaira.uchronie.TargetRange
import com.tomykaira.constraintscala.{Constraint, FSM}

class CommitsController(fsm: FSM[GraphState], currentRange: Constraint[Option[TargetRange]], dispatch: Operation => Unit)
  extends GridPanel(1, 2) {
  maximumSize = new Dimension(Integer.MAX_VALUE, 50)
  contents += new Button("Squash") {
    reactions += {
      case e: ButtonClicked =>
        currentRange.get.foreach { range =>
          dispatch(Operation.SquashOp(range, None))
        }
    }
  }
  contents += new Button("Delete") {
    reactions += {
      case e: ButtonClicked => currentRange.get.foreach { range =>
        range.list foreach {c => dispatch(Operation.DeleteOp(c))}
      }
    }
  }
  contents += new Button("Edit") {
    tooltip = "Edit is available when you have no pending operations"
    fsm.onChange {
      case GraphState.Clean(_) => enabled = true
      case _ => enabled = false
    }
    reactions += {
      case e: ButtonClicked => currentRange.get.foreach { range =>
        fsm changeState {
          case GraphState.Clean(g) => GraphState.Editing(new EditManager(g, range))
        }
      }
    }
  }
}

