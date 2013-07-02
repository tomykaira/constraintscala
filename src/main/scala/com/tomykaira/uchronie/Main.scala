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
    def updateGraphWithRearranged(result: Either[String, ArrangingGraph]) {
      result match {
        case Left(err) => Dialog.showMessage(title = "Error", message = err)
        case Right(g)  => graphConstraint.update(g)
      }
    }
    val commitsTable = new CommitsTable(graphConstraint)

    commitsTable.state.onChange({
      case commitsTable.Dropped(range, at) =>
        commitsTable.state.changeStateTo(commitsTable.RowsSelected(range))
        updateGraphWithRearranged(graphConstraint.get.reorder(range, at))
      case _ =>
    })

    val changedFiles = new FileList(commitsTable.state.convert({
      case commitsTable.RowsSelected(range) =>
        range.first.map(c => repository.diff(c)).getOrElse(Nil)
      case _ => Nil
    }))
    val comment = new CommentArea(commitsTable.state.convert({
      case commitsTable.RowsSelected(range) =>
        range.first
      case _ => None
    }))
    comment.editFSM.onChange({
      case comment.Committing(commit, message) =>
        val result = graphConstraint.get.updateComment(commit, message)
        updateGraphWithRearranged(result)
      case _ =>
    })
    val changes = new TextArea() {
      editable = false
      changedFiles.selectedItem.onChange({
        case Some(diff) => text = new DiffDecorator(diff).formatted(repository)
        case None =>
      })
    }

    val commitsController = new BorderPanel() {
      val buttons = new GridPanel(1, 2) {
        maximumSize = new Dimension(Integer.MAX_VALUE, 50)
        contents += new Button("Squash") {
          reactions += {
            case e: ButtonClicked =>
              commitsTable.state.get match {
                case commitsTable.RowsSelected(range) =>
                  updateGraphWithRearranged(range.squash())
                case _ =>
              }
          }
        }
        contents += new Button("Delete") {
          reactions += {
            case e: ButtonClicked =>
              commitsTable.state.get match {
                case commitsTable.RowsSelected(range) =>
                  updateGraphWithRearranged(range.delete())
                case _ =>
              }
          }
        }
      }
      add(commitsTable, BorderPanel.Position.Center)
      add(buttons, BorderPanel.Position.South)
    }

    contents = new SplitPane(Orientation.Vertical,
      new SplitPane(Orientation.Horizontal, commitsController, comment) {
        dividerLocation = 200
      },
      new SplitPane(Orientation.Horizontal, changedFiles, changes) {
        dividerLocation = 200
      })
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
