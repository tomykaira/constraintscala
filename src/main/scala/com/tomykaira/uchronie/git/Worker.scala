package com.tomykaira.uchronie.git

import akka.actor.{Status, Actor}

class Worker extends Actor {
  def receive = {
    case g: ArrangingGraph.Modified =>
      respondResult(g.applyCurrentThread)
    case g: ArrangingGraph.Clean =>
      respondResult(Right(g))
  }

  def respondResult(result: Either[String, ArrangingGraph]) {
    sender ! (result match {
      case Right(graph) => Status.Success(graph)
      case Left(err) => Status.Failure(new Throwable(err))
    })
  }
}
