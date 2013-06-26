package com.tomykaira.constraintscala

import com.tomykaira.constraintscala.graph.Graph

object DependencyGraph extends Graph[ChainedCache] {
  type Node = ChainedCache
  var timestamp: Int = 0

  def countUp() { timestamp += 1 }

  def onStack[A](constraint: Node, value: => A): A = {
    if (stack.isEmpty)
      countUp()
    registerDependency(constraint)
    pushStack(constraint)
    try {
      value
    } finally {
      popStack()
    }
  }

  private var stack: List[Node] = List()

  private def registerDependency(constraint: Node) {
    stack match {
      case last :: _ => drawEdge(constraint, last)
      case List() => ()
    }
  }

  private def pushStack(constraint: Node) {
    stack = constraint :: stack
  }

  private def popStack() {
    stack = stack.tail
  }

}
