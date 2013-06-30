package com.tomykaira.uchronie

import scala.swing._
import javax.swing.table.DefaultTableModel
import java.io.File
import org.eclipse.jgit.lib.ObjectId

object Main extends SimpleSwingApplication {
  def top: Frame = new MainFrame() {
    title = "Uchronie"

    val commitsTable = new Table() {
      override lazy val model = super.model.asInstanceOf[DefaultTableModel]
      autoResizeMode = Table.AutoResizeMode.LastColumn

      model addColumn "SHA1"
      model addColumn "Comment"

      peer.getColumnModel.getColumn(0).setMaxWidth(100)

      repository.listCommits(start, end).foreach(commit =>
        model addRow new CommitDecorator(commit).tableRow(repository)
      )
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
  var start: ObjectId = _
  var end: ObjectId = _

  override def main(args: Array[String]) {
    new ArgumentParser(args).parse match {
      case Left(e) => sys.error(e)
      case Right(parsed) =>
        repository = new GitRepository(parsed.repository)
        repository.resolve(parsed.start) match {
          case Some(id) => start = id
          case None => sys.error("Start SHA-1 " + parsed.start + " is not resolved to one object id")
        }
        repository.resolve(parsed.end) match {
          case Some(id) => end = id
          case None => sys.error("End SHA-1 " + parsed.end + " is not resolved to one object id")
        }
    }
    super.main(args)
  }
}
