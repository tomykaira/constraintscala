package com.tomykaira.uchronie

import org.eclipse.jgit.revwalk.RevCommit
import org.eclipse.jgit.lib.ObjectId

class ArrangingGraph(val repository: GitRepository, val start: ObjectId, val commits: List[RevCommit]) {
  def apply(nth: Int): Option[RevCommit] = {
    if (commits.indices.contains(nth))
      Some(commits(nth))
    else
      None
  }

  def updateComment(target: RevCommit, message: String): Either[String, ArrangingGraph] = {
    if (!commits.contains(target))
      return Left(target.getId.name + " is not a member of the graph")
    repository.updateComment(target, message).fold(l => Left(l), newCommit =>
      Right(repository.listCommits(start, newCommit))
    )
  }
}
