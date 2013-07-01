package com.tomykaira.uchronie

import scala.swing._
import org.eclipse.jgit.lib.ObjectId
import com.tomykaira.constraintscala.StaticConstraint
import scala.swing.event.ButtonClicked

object Main extends SimpleSwingApplication {
  def top: Frame = new MainFrame() {
    title = "Uchronie"

    val graph = repository.listCommits(start, end)
    val graphConstraint = new StaticConstraint[ArrangingGraph](graph)
    val commitsTable = new CommitsTable(graphConstraint)

    val changedFiles = new FileList(commitsTable.selectedCommit.convert({
      case Some(commit) => repository.diff(commit)
      case None => Nil
    }))
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
      changedFiles.selectedItem.onChange({
        case Some(diff) => text = repository.formatDiff(diff)
        case None =>
      })
    }

    val commitsController = new BoxPanel(Orientation.Vertical) {
      contents += commitsTable
      contents += new GridPanel(1, 2) {
        contents += new Button("Update message (Ctrl+Enter)") {
          reactions += {
            case e: ButtonClicked => comment.editFSM.startCommit()
          }
        }
        contents += new Button("Squash") {
          reactions += {
            case e: ButtonClicked =>
              commitsTable.selectedRange.get.squash() match {
                case Left(err) =>
                  Dialog.showMessage(title = "Error", message = err)
                case Right(next) =>
                  graphConstraint.update(next)
              }
          }
        }
      }
    }

    contents = new SplitPane(Orientation.Vertical,
      new SplitPane(Orientation.Horizontal, commitsController, comment),
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
