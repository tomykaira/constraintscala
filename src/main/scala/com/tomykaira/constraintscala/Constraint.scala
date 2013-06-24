package com.tomykaira.constraintscala

class Constraint[+A](getter: => A) {
  type Node = Constraint[Any]
  var callbackList = List[Any => Unit]()
  val graph = ConstraintGraph
  var cachedValue: Option[Any] = None
  var cachedAt: Option[Int] = None

  ConstraintGraph.register(this)

  def cache(value: Any) {
    cachedValue = Some(value)
    cachedAt = Some(graph.timestamp)
  }

  def onChange(callback: Any => Unit) {
    callbackList = callback :: callbackList
    callback(get)
  }

  def get : Any = ConstraintGraph.onStack[Any](this, {
    cachedValue.getOrElse({
      val v = getter
      cache(v)
      v
    })})

  def invalidate() {
    cachedValue = None
    cachedAt = None
    dependants.foreach(node => node.invalidate())
    callbackList.foreach(callback => callback(get))
  }

  private def dependants: Seq[Node] = {
    graph.dependants(this)
  }
}
