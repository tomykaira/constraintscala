package io.github.tomykaira.uchronie.git

import akka.actor.{Status, Actor}
import io.github.tomykaira.uchronie.CherryPickFailure
import io.github.tomykaira.uchronie.ui.EditManager

class Worker extends Actor {
  def receive = {
    case g: ArrangingGraph.Modified =>
      respondResult(g.applyCurrentThread)
    case g: ArrangingGraph.Clean =>
      respondResult(Right(g))
    case manager: EditManager =>
      respondResult(Right(manager.run))
  }

  def respondResult(result: Either[CherryPickFailure, ArrangingGraph]) {
    sender ! (result match {
      case Right(graph) => Status.Success(graph)
      case Left(failure) => Status.Failure(failure)
    })
  }
}
