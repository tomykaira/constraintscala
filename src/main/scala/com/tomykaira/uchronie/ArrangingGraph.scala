package com.tomykaira.uchronie

import org.eclipse.jgit.revwalk.RevCommit
import org.eclipse.jgit.lib.ObjectId
import org.eclipse.jgit.api.CheckoutResult

class ArrangingGraph(val repository: GitRepository, val start: ObjectId, val commits: List[RevCommit]) {
  val last = commits.lastOption

  def apply(nth: Int): Option[RevCommit] = {
    if (commits.indices.contains(nth))
      Some(commits(nth))
    else
      None
  }

  def updateComment(target: RevCommit, message: String): Either[String, ArrangingGraph] = {
    val temporaryBranchName = "temp" + hashCode()
    commits.indexOf(target) match {
      case -1 => Left("Target " + target.getName + " not included in current list")
      case index => {
        val orphans = commits.take(index)
        repository.checkoutAs(target, temporaryBranchName).right.map { _ =>
          val newHead = repository.amendMessage(message)
          orphans.foldRight[Either[String, RevCommit]](Right(newHead))(
            (c, prev) => prev.right.map(_ => repository.cherryPick(c)).joinRight)
        }.joinRight.right.map(newCommit => repository.listCommits(start, newCommit))
      }
    }
  }
}
