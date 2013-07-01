package com.tomykaira.constraintscala

class Constraint[+A](getter: => A) extends Notifier[A] with ChainedCache {
  private[this] var cachedValue: Option[A] = None
  private[this] var cachedAt: Option[Int] = None

  private[this] def cache(value: A) {
    cachedValue = Some(value)
    cachedAt = Some(graph.timestamp)
  }

  def get : A = graph.onStack[A](this, {
    cachedValue.getOrElse({
      val v = getter
      cache(v)
      v
    })})

  override def invalidate() {
    cachedValue = None
    cachedAt = None
    super[ChainedCache].invalidate()
    invokeCallbacks()
  }

  def convert[T](method: A => T): Constraint[T] =
    new Constraint[T]({ method(get) })
}
