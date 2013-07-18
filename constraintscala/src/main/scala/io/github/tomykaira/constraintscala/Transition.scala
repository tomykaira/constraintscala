package io.github.tomykaira.constraintscala

class Transition[S](clazz: Class[_], fsm: FSM[S], source: scala.swing.Reactor, from: S, to: S) {
  source.reactions += {case e if clazz.isInstance(e) => fsm.changeState(from, to)}
}
