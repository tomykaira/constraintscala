package com.tomykaira.uchronie.git

import com.tomykaira.uchronie.{GraphRange, ArrangingGraph}
import org.eclipse.jgit.revwalk.RevCommit

sealed trait Command

case class UpdateComment(graph: ArrangingGraph, target: RevCommit, newMessage: String) extends Command
case class Reorder(graph: ArrangingGraph, range: GraphRange, pos: Int) extends Command
case class Squash(graph: ArrangingGraph, range: GraphRange, newMessage: Option[String]) extends Command
case class Delete(graph: ArrangingGraph, range: GraphRange) extends Command


