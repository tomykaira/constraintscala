package io.github.tomykaira.uchronie.git

import io.github.tomykaira.uchronie.TargetRange
import scala.annotation.tailrec

abstract class EditManager(val graph: ArrangingGraph.Clean, range: TargetRange) {
  sealed trait UserResponse
  case object Abort extends UserResponse
  case object Continue extends UserResponse

  def abort: ArrangingGraph.Clean =
    graph.rollback

  def finish(done: IncrementalEditor.Done): ArrangingGraph.Clean =
    ArrangingGraph.renew(graph, done.head)

  def onEditWaiting: UserResponse

  def onConflictFixWaiting: UserResponse

  @tailrec
  private def loop(next: IncrementalEditor): ArrangingGraph.Clean = next match {
    case done: IncrementalEditor.Done =>
      finish(done)
    case going: IncrementalEditor.Going =>
      going.continue match {
        case done: IncrementalEditor.Done => finish(done)
        case rest: IncrementalEditor.Going =>
          onConflictFixWaiting match {
            case Continue => loop(rest)
            case Abort  => abort
          }
      }
  }

  def run: ArrangingGraph.Clean = {
    val going = graph.startEdit(range.end)

    onEditWaiting match {
      case Continue => loop(going)
      case Abort => abort
    }
  }
}
