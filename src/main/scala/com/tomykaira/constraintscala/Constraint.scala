package com.tomykaira.constraintscala

class Constraint[+A](getter: => A) extends Notifier[A] {
  type Node = Constraint[Any]
  val graph = ConstraintGraph
  private[this] var cachedValue: Option[A] = None
  private[this] var cachedAt: Option[Int] = None

  ConstraintGraph.register(this)

  private[this] def cache(value: A) {
    cachedValue = Some(value)
    cachedAt = Some(graph.timestamp)
  }

  def get : A = ConstraintGraph.onStack[A](this, {
    cachedValue.getOrElse({
      val v = getter
      cache(v)
      v
    })})

  def invalidate() {
    cachedValue = None
    cachedAt = None
    dependants.foreach(node => node.invalidate())
    invokeCallbacks()
  }

  private def dependants: Seq[Node] = {
    graph.dependants(this)
  }
}
