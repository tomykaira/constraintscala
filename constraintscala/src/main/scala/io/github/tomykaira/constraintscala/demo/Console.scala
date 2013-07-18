package io.github.tomykaira.constraintscala.demo

import scala.swing._
import javax.swing.border.BevelBorder
import scala.swing.event.ButtonClicked
import scala.sys.process.{ProcessIO, Process}
import scala.io.Source
import io.github.tomykaira.constraintscala.{Constraint, Binding, FSM}
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
      }
      val outputBuffer = new StringBuilder
      val outputConstraint = new Constraint[String]({ outputBuffer.toString() })
      val insertOutput = (str: String) => {
        outputBuffer append str+"\n"
        outputConstraint.invalidate()
      }
      Binding.text(resultArea, outputConstraint)
      val runButton = new Button("Run") {
        reactions += {
          case e: ButtonClicked => {
            insertOutput("Running...")
            new ProcessManager(commandArea.text,
              {
                case Success(c) if c == 0 => execFSM.changeState(Running(), Completed())
                case _ => execFSM.changeState(Running(), Failed())
              }, insertOutput).start()
            execFSM.changeStateTo(Running())
          }
        }
      }
      val isRunningMatcher: PartialFunction[ExecutionState, Boolean] = {
        case _: Running => false
        case _ => true
      }
      execFSM.setOnChange[Boolean](commandArea.editable = _, isRunningMatcher)
      execFSM.setOnChange[Boolean](runButton.enabled = _, isRunningMatcher)
      Binding.background(resultArea, execFSM.convert[java.awt.Color]({
        case _: Running   => java.awt.Color.WHITE
        case _: Completed => java.awt.Color.LIGHT_GRAY
        case _: Failed    => java.awt.Color.RED
      }))
      contents += (commandArea, runButton, resultArea)
    }
  }
}

sealed trait ExecutionState
case class Running() extends ExecutionState
case class Completed() extends ExecutionState
case class Failed() extends ExecutionState

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
