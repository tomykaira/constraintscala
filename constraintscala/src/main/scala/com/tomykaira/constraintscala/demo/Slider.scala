package com.tomykaira.constraintscala.demo

import scala.swing._
import com.tomykaira.constraintscala.{Binding, Constraint}
import scala.swing.event.ValueChanged

object Slider extends SimpleSwingApplication {
  class HexSlider extends Slider {
    min = 0
    max = 255
  }

  class RGBColor(val r: Int, val g: Int, val b: Int) {
    override def toString =
      "#" + Integer.toHexString(r) +
        Integer.toHexString(g) +
        Integer.toHexString(b)
  }

  implicit def awtColor(color: RGBColor): java.awt.Color =
    new java.awt.Color(color.r, color.g, color.b)

  def top: Frame = new MainFrame {
    title = "Slider Demo"

    contents = new BoxPanel(Orientation.Vertical) {
      val colorCodeLabel = new Label("#000000")
      val coloredPanel = new GridPanel(3, 2) {
        val addColor = (initialValue : Int) => {
          val label = new Label(initialValue.toString)
          val slider = new HexSlider() { value = initialValue }
          val constraint = new Constraint[Int]({ slider.value })
          Binding.text(label, constraint)
          slider.reactions += { case e: ValueChanged => constraint.invalidate() }
          constraint onChange { v => label.text = v.toString }
          contents += label
          contents += slider
          constraint
        }
        val redConstraint = addColor(128)
        val greenConstraint = addColor(128)
        val blueConstraint = addColor(128)

        val color = new Constraint({
          new RGBColor(redConstraint.get, greenConstraint.get, blueConstraint.get)
        })

        Binding.text(colorCodeLabel, color)
        color.onChange(background = _)
      }

      contents += colorCodeLabel
      contents += coloredPanel
    }
  }
}
