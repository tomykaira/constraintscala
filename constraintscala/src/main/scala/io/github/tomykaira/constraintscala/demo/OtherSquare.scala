package io.github.tomykaira.constraintscala.demo

import scala.swing._
import io.github.tomykaira.constraintscala.{Binding, FSM}
import scala.swing.event.{MouseReleased, MousePressed, MouseExited, MouseEntered}

object OtherSquare extends SimpleSwingApplication {

  def top: Frame = new MainFrame {
    title = "Other Square Demo"

    contents = new BoxPanel(Orientation.Vertical) {

      val createStateFor = (area: TextField) => {
        new FSM[AreaState] {
          state = Idle()
          override val transitions = List(
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
        Binding.background(area, fsm.convert[java.awt.Color]({
          case Idle()    => java.awt.Color.black
          case Hover()   => java.awt.Color.green
          case Pressed() => java.awt.Color.red
        }))
      }

      val a = new TextField("TextField A") {
        listenTo( mouse.clicks, mouse.moves )
      }
      val b = new TextField("TextField B") {
        listenTo( mouse.clicks, mouse.moves )
      }

      val fsmA = createStateFor(a)
      val fsmB = createStateFor(b)

      registerBackgroundCallback(fsmA, b)
      registerBackgroundCallback(fsmB, a)

      contents += (a, b)
    }
  }
}

sealed trait AreaState
case class Idle() extends AreaState
case class Hover() extends AreaState
case class Pressed() extends AreaState
