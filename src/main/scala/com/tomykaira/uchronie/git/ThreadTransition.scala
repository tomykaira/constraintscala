package com.tomykaira.uchronie.git

import scala.collection.mutable.ListBuffer

class ThreadTransition(initial: CommitThread) {
  var current: CommitThread = initial

  val history: ListBuffer[(CommitThread, Operation)] = ListBuffer()

  def transit(op: Operation): CommitThread = {
    val next = current.applyOperation(op)
    (current, op) +=: history
    current = next
    next
  }

  def pop() = {
    val (thread, _) = history.remove(0)
    current = thread
  }

  def threads: List[CommitThread] = current :: history.map(_._1).toList

  def operations: List[Operation] = history.map(_._2).toList
}
