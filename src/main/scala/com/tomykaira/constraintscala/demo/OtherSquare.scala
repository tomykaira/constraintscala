package com.tomykaira.constraintscala.demo

import scala.swing._
import com.tomykaira.constraintscala.{Transition, FSM}
import scala.swing.event.{MouseReleased, MousePressed, MouseExited, MouseEntered}

object OtherSquare extends SimpleSwingApplication {

  def top: Frame = new MainFrame {
    title = "Other Square Demo"

    contents = new BoxPanel(Orientation.Vertical) {

      val createStateFor = (area: TextField) => {
        new FSM[AreaState] {
          state = Idle()
          val transitions = List(
            transition[MouseEntered](area, Idle(), Hover()),
            transition[MouseEntered](area, Idle(), Hover()),
            transition[MouseExited](area, Hover(), Idle()),
            transition[MousePressed](area, Hover(), Pressed()),
            transition[MouseReleased](area, Pressed(), Hover()),
            transition[MouseExited](area, Pressed(), Idle())
          )
        }
      }

      val registerBackgroundCallback = (fsm: FSM[AreaState], area: TextField) => {
        fsm.onChange({
          case Idle()    => area.background = java.awt.Color.black
          case Hover()   => area.background = java.awt.Color.green
          case Pressed() => area.background = java.awt.Color.red
        })
      }

      val a = new TextField("TextField A") {
        listenTo( mouse.clicks, mouse.moves )
      }
      val b = new TextField("TextField B") {
        listenTo( mouse.clicks, mouse.moves )
      }

      a.reactions += {case e if MousePressed.getClass.isInstance(e) => println("Exit") }

      val fsmA = createStateFor(a)
      val fsmB = createStateFor(b)

      registerBackgroundCallback(fsmA, b)
      registerBackgroundCallback(fsmB, a)

      contents += a
      contents += b
    }
  }
}

sealed trait AreaState
case class Idle() extends AreaState
case class Hover() extends AreaState
case class Pressed() extends AreaState
