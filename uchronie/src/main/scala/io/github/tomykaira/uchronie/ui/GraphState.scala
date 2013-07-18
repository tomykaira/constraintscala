package io.github.tomykaira.uchronie.ui

import io.github.tomykaira.uchronie.git.ArrangingGraph

sealed trait GraphState {
  val graph: ArrangingGraph
}

object GraphState {
  def apply(graph: ArrangingGraph): GraphState = graph match {
    case modified: ArrangingGraph.Modified => Modified(modified)
    case clean: ArrangingGraph.Clean => Clean(clean)
  }

  case class Clean(graph: ArrangingGraph.Clean) extends GraphState
  case class Modified(graph: ArrangingGraph.Modified) extends GraphState
  case class Applying(graph: ArrangingGraph.Modified) extends GraphState
  case class Editing(manager: EditManager) extends GraphState{
    val graph: ArrangingGraph.Clean = manager.graph
  }
}