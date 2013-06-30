package com.tomykaira.uchronie

import scala.swing.Table
import scala.swing.event.TableRowsSelected
import javax.swing.table.DefaultTableModel
import org.eclipse.jgit.lib.ObjectId
import com.tomykaira.constraintscala.Constraint
import org.eclipse.jgit.revwalk.RevCommit

class CommitsTable(repository: GitRepository, start: ObjectId, end: ObjectId) extends Table {
  override lazy val model = super.model.asInstanceOf[DefaultTableModel]
  autoResizeMode = Table.AutoResizeMode.LastColumn

  listenTo(selection)

  model addColumn "SHA1"
  model addColumn "Comment"

  peer.getColumnModel.getColumn(0).setMaxWidth(100)

  val graph = repository.listCommits(start, end)
  val selectedCommit = new Constraint[Option[RevCommit]]({
    graph(peer.getSelectedRow)
  })

  reactions += {
    case e: TableRowsSelected if !e.adjusting => selectedCommit.invalidate()
  }

  graph.commits.foreach(commit =>
    model addRow new CommitDecorator(commit).tableRow(repository)
  )
}
