package io.github.tomykaira.constraintscala.graph

trait Graph[N] {
  var nodes: List[N] = List()
  var edges: List[Edge[N]] = List()

  def register(node: N) {
    nodes = node :: nodes
  }

  def drawEdge(from: N, to: N) {
    if (edges.find(e => e.from == from && e.to == to).isEmpty) {
      val edge = new Edge[N](from, to)
      edges = edge :: edges
    }
  }

  def dependants(node: N): Seq[N] = {
    edges.filter(e => e.from == node).map(_.to)
  }
}
