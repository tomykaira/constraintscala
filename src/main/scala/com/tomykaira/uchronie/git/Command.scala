package com.tomykaira.uchronie.git

import com.tomykaira.uchronie.{GraphRange, ArrangingGraph}
import org.eclipse.jgit.revwalk.RevCommit

sealed trait Command

case class UpdateComment(graph: ArrangingGraph, target: RevCommit, newMessage: String) extends Command
case class Reorder(graph: ArrangingGraph, range: GraphRange, pos: Int) extends Command


