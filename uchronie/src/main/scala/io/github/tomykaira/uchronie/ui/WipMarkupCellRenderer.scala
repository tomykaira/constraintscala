package io.github.tomykaira.uchronie.ui

import javax.swing.table.DefaultTableCellRenderer
import javax.swing.JTable
import java.awt.{Component, Color}

class WipMarkupCellRenderer extends DefaultTableCellRenderer {
  private[this] val lightGreen: Color = new Color(0xe0, 0xfc, 0xb6)

  override def getTableCellRendererComponent(table: JTable,
                                             value: Any,
                                             isSelected: Boolean,
                                             hasFocus: Boolean,
                                             row: Int,
                                             column: Int): Component = {
    val base = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)

    val comment = table.getModel.getValueAt(row, 1)

    val color =
      if (isSelected || hasFocus)
        table.getSelectionBackground
      else if (comment.asInstanceOf[String].startsWith("WIP:"))
        lightGreen
      else
        table.getBackground

    base.setBackground(color)

    base
  }
}
