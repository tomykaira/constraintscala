package com.tomykaira.constraintscala

import com.tomykaira.constraintscala.graph.{Graph}

object ConstraintGraph extends Graph[Constraint[Any]] {
  var timestamp: Int = 0

  def countUp() { timestamp += 1 }

  def onStack[A](constraint: Constraint[Any], value: => A): A = {
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

  private var stack: List[Constraint[Any]] = List()

  private def registerDependency(constraint: Constraint[Any]) {
    stack match {
      case last :: _ => drawEdge(constraint, last)
      case List() => ()
    }
  }

  private def pushStack(constraint: Constraint[Any]) {
    stack = constraint :: stack
  }

  private def popStack() {
    stack = stack.tail
  }

}
