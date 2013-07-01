package com.tomykaira.constraintscala

class StaticConstraint[A](initialValue: => A) extends Notifier[A] with ChainedCache {
  private[this] var currentValue: A = initialValue
  def get: A = graph.onStack(this, currentValue)

  def update(value: A) {
    currentValue = value
    super[ChainedCache].invalidate()
    invokeCallbacks()
  }

  override def invalidate() {
    // this constraint depends no other constraint
  }
}
