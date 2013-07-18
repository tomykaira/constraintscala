package io.github.tomykaira.constraintscala

trait FSM[S] extends Notifier[S] {
  var state: S = _
  val transitions: List[Transition[S]] = List()
  def get: S = state

  def changeState(from: S, to: S) {
    if(state == from)
      changeStateTo(to)
  }

  def changeState(map: PartialFunction[S,S]) {
    if (map.isDefinedAt(get))
      changeStateTo(map(get))
  }

  def setOnChange[V](setter: V => Unit, stateToValue: S => V) {
    onChange(s => setter(stateToValue(s)))
  }

  def changeStateTo(to: S) {
    state = to
    invokeCallbacks()
  }

  def transition[E <: swing.event.Event](source: scala.swing.Reactor, from: S, to: S)(implicit m: Manifest[E]) =
    new Transition[S](m.runtimeClass, this, source, from, to)

  def convert[T](method: S => T): Constraint[T] = {
    val constraint = new Constraint[T]({ method(get) })
    onChange(s => constraint.invalidate())
    constraint
  }

}
