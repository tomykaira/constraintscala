package com.tomykaira.uchronie.ui

import javax.swing.border.EmptyBorder
import scala.swing.{Component, ScrollPane}

object SwingHelper {
  def scrollable(c: Component): ScrollPane =
    new ScrollPane(c) {
      border = new EmptyBorder(0,0,0,0)
    }
}
