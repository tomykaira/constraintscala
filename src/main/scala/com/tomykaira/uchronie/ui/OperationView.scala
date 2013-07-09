package com.tomykaira.uchronie.ui

import com.tomykaira.constraintscala.{StaticConstraint, FSM}
import com.tomykaira.uchronie.Main.{Stopped, Working, ProcessingState}
import scala.swing._
import java.awt.Dimension
import com.tomykaira.uchronie.ArrangingGraph
import com.tomykaira.uchronie.git.Operation
import scala.swing.event.ButtonClicked

class OperationView(processingFSM: FSM[ProcessingState],
    graph: StaticConstraint[ArrangingGraph],
    onApply: () => Any) extends BorderPanel {
  val list = new ScrollPane() {
    contents = new ListView[Operation] {
      graph.onChange { g =>
        listData = g.transition.operations
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
    reactions += {
      case e: ButtonClicked => onApply()
    }
  }

  val undoButton = new Button("Undo") {
    preferredSize = new Dimension(200, 50)
    maximumSize = preferredSize
    minimumSize = preferredSize
    tooltip = "Undo a change"
    reactions += {
      case e: ButtonClicked => {
        graph.get.transition.pop()
        graph.update(graph.get)
      }

    }
  }

  val progressBar = new ProgressBar {
    processingFSM.onChange {
      case Working() => indeterminate = true
      case Stopped() => indeterminate = false
    }
  }

  add(list, BorderPanel.Position.Center)
  add(new BoxPanel(Orientation.Horizontal) { contents += (applyButton, undoButton) }, BorderPanel.Position.East)
  add(progressBar, BorderPanel.Position.South)
}
