package com.tomykaira.constraintscala.demo

import scala.swing._
import scala.collection.mutable

object Slider extends SimpleSwingApplication {
  def top: Frame = new MainFrame {
    title = "Slider Demo"

    contents = new BoxPanel(Orientation.Vertical) {
      val colorCodeLabel = new Label("#000000")
      val coloredPanel = new GridPanel(3, 2) {
        val redLabel = new Label("128")
        val greenLabel = new Label("128")
        val blueLabel = new Label("128")

        val redSlider = new Slider()
        val greenSlider = new Slider()
        val blueSlider = new Slider()

        Seq(redLabel, redSlider, greenLabel, greenSlider, blueLabel, blueSlider).foreach(contents += _)
      }

      contents += colorCodeLabel
      contents += coloredPanel
    }
  }
}
