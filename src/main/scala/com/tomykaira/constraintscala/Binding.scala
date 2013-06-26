package com.tomykaira.constraintscala

import scala.swing.{Label, TextComponent}

object Binding {
  def text(field: TextComponent, constraint: Constraint[Any]) {
    constraint.onChange(value => field.text = value.toString)
  }

  def text(field: Label, constraint: Constraint[Any]) {
    constraint.onChange(value => field.text = value.toString)
  }
}
