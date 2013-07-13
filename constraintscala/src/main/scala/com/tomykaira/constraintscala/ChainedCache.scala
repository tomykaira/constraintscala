package com.tomykaira.constraintscala

trait ChainedCache {
  val graph = DependencyGraph

  graph.register(this)

  def invalidate() {
    dependants.foreach(node => node.invalidate())
  }

  private def dependants: Seq[DependencyGraph.Node] = {
    graph.dependants(this)
  }
}
