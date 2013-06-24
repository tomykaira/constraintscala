package com.tomykaira.constraintscala.demo

import scala.swing._
import com.tomykaira.constraintscala.Constraint
import scala.swing.event.ValueChanged

object Slider extends SimpleSwingApplication {
  def top: Frame = new MainFrame {
    title = "Slider Demo"

    contents = new BoxPanel(Orientation.Vertical) {
      val colorCodeLabel = new Label("#000000")
      val coloredPanel = new GridPanel(3, 2) {
        val addColor = (initialValue : Int) => {
          val label = new Label(initialValue.toString)
          val slider = new HexSlider() { value = initialValue }
          val constraint = new Constraint[Int]({ slider.value })
          constraint.onChange((value : Int) => {label.text = value.toString})
          slider.reactions += { case e: ValueChanged => constraint.invalidate() }
          contents += label
          contents += slider
          constraint
        }
        val redConstraint = addColor(128)
        val greenConstraint = addColor(128)
        val blueConstraint = addColor(128)

        val hex = new Constraint({
          "#" + Integer.toHexString(redConstraint.get) +
            Integer.toHexString(greenConstraint.get) +
            Integer.toHexString(blueConstraint.get)
        })

        val color = new Constraint[java.awt.Color]({
          new java.awt.Color(redConstraint.get,
            greenConstraint.get,
            blueConstraint.get)
        })

        hex.onChange((value : String) => colorCodeLabel.text = value)
        color.onChange((value : java.awt.Color) => background = value)
      }

      contents += colorCodeLabel
      contents += coloredPanel
    }
  }

  class HexSlider extends Slider {
    min = 0
    max = 255
  }
}
