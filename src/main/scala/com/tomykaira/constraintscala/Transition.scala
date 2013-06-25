package com.tomykaira.constraintscala

class Transition[S](klass: Class[_], aFSM: FSM[S], source: scala.swing.Reactor, from: S, to: S) {
  val fsm: FSM[S] = aFSM
  source.reactions += {case e if klass.isInstance(e) => fsm.changeState(from, to)}
}
