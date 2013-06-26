package com.tomykaira.constraintscala

import scala.swing.{UIElement, Label, TextComponent}

object Binding {
  def text(field: TextComponent, constraint: Notifier[Any]) {
    constraint.onChange(value => field.text = value.toString)
  }

  def text(field: Label, constraint: Notifier[Any]) {
    constraint.onChange(value => field.text = value.toString)
  }

  def background(field: UIElement, constraint: Notifier[java.awt.Color]) {
    constraint.onChange(field.background = _)
  }
}
