package com.tomykaira.uchronie

import scala.swing._
import org.eclipse.jgit.lib.ObjectId
import com.tomykaira.uchronie.ui._
import com.tomykaira.uchronie.ui.SwingHelper._

object Main extends SimpleSwingApplication {
  def top: Frame = new MainFrame() {
    title = "Uchronie"

    private[this] val fsm = new GraphFSM(repository, start, end)

    private[this] val commitsTable = new CommitsTable(fsm)

    private[this] val comment = new CommentArea(fsm, commitsTable.currentRange)

    private[this] val commitsView = new BorderPanel {
      add(scrollable(commitsTable), BorderPanel.Position.Center)
      add(new CommitsController(fsm, commitsTable.currentRange), BorderPanel.Position.South)
    }

    private[this] val gitView = new SplitPane(Orientation.Vertical,
      new SplitPane(Orientation.Horizontal, commitsView, scrollable(comment)) {
        dividerLocation = 200
      }, new ChangesView(fsm, commitsTable.currentRange))

    contents = new BorderPanel() {
      add(new OperationView(fsm), BorderPanel.Position.North)
      add(gitView, BorderPanel.Position.Center)
    }

    override def closeOperation() {
      repository.resetToOriginalBranch(fsm.get.graph.last)
      super.closeOperation()
    }
  }

  var repository: GitRepository = _
  var start: ObjectId = _
  var end: ObjectId = _

  override def main(args: Array[String]) {
    new ArgumentParser(args).parse match {
      case Left(e) => initializationError(e, None)
      case Right(parsed) =>
        repository = new GitRepository(parsed.repository)
        if (!repository.isClean)
          initializationError("Repository is not clean.  Commit everything before start uchronie for safety.", Some(repository))
        repository.resolve(parsed.start) match {
          case Some(id) => start = id
          case None => initializationError("Start SHA-1 " + parsed.start + " is not resolved to one object id", Some(repository))
        }
        repository.resolve(parsed.end) match {
          case Some(id) => end = id
          case None => initializationError("End SHA-1 " + parsed.end + " is not resolved to one object id", Some(repository))
        }
    }
    super.main(args)
  }

  def initializationError(message: String, repo: Option[GitRepository]) {
    repo foreach { repo => repo.resetToOriginalBranch(repo.head) }
    System.err.println(message)
    System.exit(1)
  }
}
