package com.tomykaira.constraintscala

class Transition[S](klass: Class[_], fsm: FSM[S], source: scala.swing.Reactor, from: S, to: S) {
  source.reactions += {case e if klass.isInstance(e) => fsm.changeState(from, to)}
}
