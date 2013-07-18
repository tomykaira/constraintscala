package io.github.tomykaira.constraintscala

import scala.swing.{Component, UIElement, Label, TextComponent}

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

  def enabled(field: Component, constraint: Notifier[Boolean]) {
    constraint.onChange(field.enabled = _)
  }
}
