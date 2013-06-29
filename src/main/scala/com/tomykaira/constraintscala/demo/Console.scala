package com.tomykaira.constraintscala.demo

import scala.swing._
import javax.swing.border.BevelBorder
import scala.swing.event.ButtonClicked
import scala.sys.process.{ProcessIO, Process}
import scala.io.Source
import com.tomykaira.constraintscala.{Binding, Transition, FSM}
import scala.concurrent.future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Try}

object Console extends SimpleSwingApplication {
  def top: Frame = new MainFrame {
    title = "Simple Console"

    contents = new GridPanel(3, 1) {
      val commandArea = new TextArea() {
        border = new BevelBorder(BevelBorder.LOWERED)
      }
      val resultArea = new TextArea {
        editable = false
        background = java.awt.Color.LIGHT_GRAY
      }
      val execFSM = new FSM[ExecutionState] {
        state = Completed()
        val transitions: List[Transition[ExecutionState]] = List()
      }
      val runButton = new Button("Run") {
        reactions += {
          case e: ButtonClicked => {
            resultArea.text = "Running...\n"
            new ProcessManager(commandArea.text,
              {
                case Success(c) if c == 0 => execFSM.changeState(Running(), Completed())
                case _ => execFSM.changeState(Running(), Failed())
              },
              line => resultArea append line+"\n").start()
            execFSM.changeStateTo(Running())
          }
        }
      }
      execFSM.onChange(s => commandArea.editable = !s.running)
      execFSM.onChange(s => runButton.enabled = !s.running)
      Binding.background(resultArea, execFSM.convert[java.awt.Color](_.color))
      contents += (commandArea, runButton, resultArea)
    }
  }
}

sealed trait ExecutionState {
  val color: java.awt.Color
  val running: Boolean
}
case class Running() extends ExecutionState {
  val color = java.awt.Color.WHITE
  val running = true
}
case class Completed() extends ExecutionState{
  val color = java.awt.Color.LIGHT_GRAY
  val running = false
}
case class Failed() extends ExecutionState{
  val color = java.awt.Color.RED
  val running = false
}

class ProcessManager(command: String, onExit: Try[Int] => Unit, out: String => Unit) {
  def start() {
    future {
      Process("sh").run(
        new ProcessIO(stdin => { stdin.write(command.getBytes("UTF-8")); stdin.close() },
          stdout => Source.fromInputStream(stdout).getLines().foreach(out),
          stderr => Source.fromInputStream(stderr).getLines().foreach(out))).exitValue()
    } onComplete onExit
  }
}
