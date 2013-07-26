package io.github.tomykaira.uchronie.git

import akka.actor.Actor

case class Request(parcel: AnyRef, onSuccess: ArrangingGraph => Unit, onFailure: RuntimeException => Unit)

class Worker extends Actor {
  def receive = {
    case Request(parcel, success, failure) =>
      parcel match {
        case g: ArrangingGraph.Modified =>
          g.applyCurrentThread match {
            case Right(graph) => success(graph)
            case Left(err) => failure(err)
          }
        case g: ArrangingGraph.Clean =>
          success(g)
        case manager: EditManager =>
          success(manager.run)
        case _ =>
          failure(new RuntimeException("Unknown request type"))
      }
  }
}
