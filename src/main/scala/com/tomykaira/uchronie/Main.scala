package com.tomykaira.uchronie

import scala.swing._
import javax.swing.table.DefaultTableModel
import java.io.File

object Main extends SimpleSwingApplication {
  def top: Frame = new MainFrame() {
    title = "Uchronie"

    val commitsTable = new Table() {
      override lazy val model = super.model.asInstanceOf[DefaultTableModel]
      autoResizeMode = Table.AutoResizeMode.LastColumn

      model addColumn "SHA1"
      model addColumn "Comment"

      model addRow Array[AnyRef]("0000000", "Hello World")
      model addRow Array[AnyRef]("0000000", "Hello World")
      model addRow Array[AnyRef]("0000000", "Hello World")
    }

    val changedFiles = new ListView[String](List("Foo.scala", "Bar.scala", "build.sbt")) {

    }
    val comment = new TextArea() {
      editable = true
      text = "Comment"
    }
    val changes = new TextArea() {
      editable = false
      text = "Changes"
    }

    contents = new SplitPane(Orientation.Vertical,
      new SplitPane(Orientation.Horizontal, commitsTable, comment),
      new SplitPane(Orientation.Horizontal, changedFiles, changes))
  }

  var repository: GitRepository = _
  var start: String = _
  var end: String = _

  override def main(args: Array[String]) {
    new ArgumentParser(args).parse match {
      case Left(e) =>
        println(e)
        sys.exit(1)
      case Right(parsed) =>
        repository = new GitRepository(parsed.repository)
        start = parsed.start
        end = parsed.end
    }
    super.main(args)
  }
}
