package io.github.tomykaira.uchronie

import scala.swing._
import org.eclipse.jgit.lib.ObjectId
import io.github.tomykaira.uchronie.ui._
import io.github.tomykaira.uchronie.ui.SwingHelper._
import scala.collection.JavaConverters._
import javax.swing.UIManager

object Main extends SimpleSwingApplication {
  def top: Frame = new MainFrame() {
    title = "Uchronie"

    private[this] val baseFont = java.awt.GraphicsEnvironment.getLocalGraphicsEnvironment.getAllFonts find { font =>
      font.getName == "Ricty Regular"
    } getOrElse font deriveFont(java.awt.Font.PLAIN, 14)

    UIManager.getDefaults.entrySet().asScala foreach { entry =>
      if (entry.getKey.toString.endsWith(".font"))
        UIManager.put(entry.getKey, baseFont)
    }

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
    System.setProperty("awt.useSystemAAFontSettings", "on")
    System.setProperty("swing.aatext", "true")

    new ArgumentParser(args).parse match {
      case Left(e) => initializationError(e)
      case Right(parsed) =>
        repository = new GitRepository(parsed.repository)
        if (!repository.isClean)
          initializationError("Repository is not clean.  Commit everything before start uchronie for safety.")
        repository.resolve(parsed.start) match {
          case Some(id) => start = id
          case None => initializationError("Start SHA-1 " + parsed.start + " is not resolved to one object id")
        }
        repository.resolve(parsed.end) match {
          case Some(id) => end = id
          case None => initializationError("End SHA-1 " + parsed.end + " is not resolved to one object id")
        }
    }
    repository.startWork()
    super.main(args)
  }

  def initializationError(message: String) {
    System.err.println(message)
    System.exit(1)
  }
}
