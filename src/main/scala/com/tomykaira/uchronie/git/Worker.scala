package com.tomykaira.uchronie.git

import akka.actor.{Status, Actor}
import com.tomykaira.constraintscala.FSM
import com.tomykaira.uchronie.ArrangingGraph

sealed trait WorkerState
case class Started() extends WorkerState
case class Succeeded(c: Command, newGraph: ArrangingGraph) extends WorkerState
case class Failed(c: Command, error: String) extends WorkerState

class Worker extends Actor {
  val state = new FSM[WorkerState] {
    state = Started()
  }

  def receive = {
    case (c @ UpdateComment(graph, target, message)) =>
      respondResult(graph.updateComment(target, message))
    case (c @ Reorder(graph, range, pos)) =>
      respondResult(graph.reorder(range, pos))
    case (c @ Squash(graph, range, message)) =>
      respondResult(graph.squash(range, message))
    case (c @ Delete(graph, range)) =>
      respondResult(graph.delete(range))
  }

  def respondResult(result: Either[String, ArrangingGraph]) {
    sender ! (result match {
      case Right(graph) => Status.Success(graph)
      case Left(err) => Status.Failure(new Throwable(err))
    })
  }
}
