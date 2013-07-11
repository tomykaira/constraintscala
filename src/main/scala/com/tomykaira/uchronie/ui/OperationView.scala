package com.tomykaira.uchronie.ui

import com.tomykaira.constraintscala.{Constraint, Binding, FSM}
import scala.swing._
import java.awt.Dimension
import com.tomykaira.uchronie.git.Operation
import scala.swing.event.ButtonClicked

class OperationView(fsm: FSM[GraphState]) extends BorderPanel {

  private[this] val modifiedConstraint: Constraint[Boolean] = fsm.convert {
    case _: GraphState.Modified => true
    case _ => false
  }

  val list = new ScrollPane() {
    contents = new ListView[Operation] {
      fsm.onChange { state =>
        listData = state.graph.history
        repaint()
      }
    }
    preferredSize = new Dimension(Int.MaxValue, 50)
  }

  val applyButton = new Button("Apply") {
    preferredSize = new Dimension(200, 50)
    maximumSize = preferredSize
    minimumSize = preferredSize
    tooltip = "Apply stacked changes to the repository"

    Binding.enabled(this, modifiedConstraint)

    reactions += {
      case e: ButtonClicked =>
        fsm changeState { case GraphState.Modified(g) => GraphState.Applying(g) }
    }
  }

  val undoButton = new Button("Undo") {
    preferredSize = new Dimension(200, 50)
    maximumSize = preferredSize
    minimumSize = preferredSize
    tooltip = "Undo a change"

    Binding.enabled(this, modifiedConstraint)

    reactions += {
      case e: ButtonClicked => {
        fsm changeState { case GraphState.Modified(g) => GraphState(g.previous) }
      }
    }
  }

  val progressBar = new ProgressBar {
    fsm.onChange {
      case GraphState.Applying(_) | GraphState.Editing(_) => indeterminate = true
      case GraphState.Clean(_) | GraphState.Modified(_) => indeterminate = false
    }
  }

  add(list, BorderPanel.Position.Center)
  add(new BoxPanel(Orientation.Horizontal) { contents += (applyButton, undoButton) }, BorderPanel.Position.East)
  add(progressBar, BorderPanel.Position.South)
}
