package com.tomykaira.constraintscala

import org.scalatest.FunSpec

class FSMSpec extends FunSpec {
  sealed trait State
  case class Init() extends State
  case class Next() extends State

  describe("convert") {
    it("should invoke callbacks of constraint when FSM state changes") {
      val fsm = new FSM[State] {
        val transitions = List()
        state = Init()
      }
      val constraint = fsm.convert[Boolean]({ s => s.isInstanceOf[Next] })
      var called = false
      constraint.onChange(called = _)
      fsm.changeStateTo(Next())
      assert(called)
    }
  }
}
