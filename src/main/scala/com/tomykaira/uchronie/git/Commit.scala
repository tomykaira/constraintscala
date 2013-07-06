package com.tomykaira.uchronie.git

import org.eclipse.jgit.revwalk.RevCommit
import scala.language.implicitConversions

object Commit {
  implicit def commitToRevCommit(commit: Commit): RevCommit = commit.raw
  implicit def revCommitToRawCommit(rev: RevCommit): Commit = RawCommit(rev)
}
sealed trait Commit {
  val raw: RevCommit
}
case class RawCommit(raw: RevCommit) extends Commit
case class Picked(raw: RevCommit, previous: Commit) extends Commit
case class Renamed(raw: RevCommit, previous: Commit) extends Commit
case class Moved(raw: RevCommit, previous: Commit) extends Commit
case class Squashed(raw: RevCommit, previous: List[Commit]) extends Commit
