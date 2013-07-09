package com.tomykaira.uchronie.ui

import com.tomykaira.constraintscala.{StaticConstraint, FSM}
import com.tomykaira.uchronie.Main.{Stopped, Working, ProcessingState}
import scala.swing._
import java.awt.Dimension
import com.tomykaira.uchronie.ArrangingGraph
import com.tomykaira.uchronie.git.Operation
import scala.swing.ListView.Renderer

class OperationView(processingFSM: FSM[ProcessingState], graph: StaticConstraint[ArrangingGraph]) extends BorderPanel {
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
  }

  val progressBar = new ProgressBar {
    processingFSM.onChange {
      case Working() => indeterminate = true
      case Stopped() => indeterminate = false
    }
  }

  add(list, BorderPanel.Position.Center)
  add(applyButton, BorderPanel.Position.East)
  add(progressBar, BorderPanel.Position.South)
}
