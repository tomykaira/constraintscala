package com.tomykaira.uchronie

import scala.swing._
import javax.swing.table.DefaultTableModel
import java.io.File
import org.eclipse.jgit.lib.{Constants, ObjectId}
import scala.swing.event.{TableRowsSelected, SelectionChanged}
import com.tomykaira.constraintscala.{StaticConstraint, Constraint}
import org.eclipse.jgit.diff.DiffEntry
import scala.swing.ListView.Renderer

object Main extends SimpleSwingApplication {
  def top: Frame = new MainFrame() {
    title = "Uchronie"

    val graph = repository.listCommits(start, end)
    val graphConstraint = new StaticConstraint[ArrangingGraph](graph)
    val commitsTable = new CommitsTable(graphConstraint)

    val changedFiles = new ListView[DiffEntry]() {
      commitsTable.selectedCommit.onChange({
        case Some(commit) => listData = repository.diff(commit); repaint()
        case None => listData = Nil; repaint()
      })

      renderer = new Renderer[DiffEntry] {
        def componentFor(list: ListView[_], isSelected: Boolean, focused: Boolean, diff: DiffEntry, index: Int): Component = {
          new Label {
            text = diff.getChangeType match {
              case DiffEntry.ChangeType.ADD | DiffEntry.ChangeType.MODIFY => diff.getNewPath
              case DiffEntry.ChangeType.DELETE => diff.getOldPath
              case DiffEntry.ChangeType.COPY | DiffEntry.ChangeType.RENAME =>
                diff.getOldPath + " -> " + diff.getNewPath
            }
          }
        }
      }
    }
    val comment = new CommentArea(commitsTable.selectedCommit)
    comment.editFSM.onChange({
      case comment.Committing(commit, message) =>
        graphConstraint.get.updateComment(commit, message) match {
          case Left(error) => Dialog.showMessage(title = "Error", message = error)
          case Right(newGraph) => {
            comment.editFSM.changeStateTo(comment.Committed())
            graphConstraint.update(newGraph)
          }
        }
      case _ =>
    })
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
