package com.tomykaira.uchronie

import org.eclipse.jgit.lib.{Constants, ObjectId}
import scala.annotation.tailrec
import com.tomykaira.uchronie.git.{CommitThread, ThreadTransition, Commit}

class ArrangingGraph(val repository: GitRepository, val start: ObjectId, val last: ObjectId) {
  type OperationResult = Either[String, ArrangingGraph]

  private lazy val startCommit: Commit = repository.toCommit(start)
  private lazy val lastCommit: Commit = repository.toCommit(last)
  lazy val commits: List[Commit.Raw] = repository.listCommits(start, last).map(Commit.Raw)
  val transition: ThreadTransition = new ThreadTransition(CommitThread.fromCommits(commits))

  rollback()

  def apply(nth: Int): Option[Commit] = {
    if (commits.indices.contains(nth))
      Some(commits(nth))
    else
      None
  }

  def rollback() {
    repository.resetHard(last)
  }

  def updateComment(target: Commit, message: String): OperationResult = {
    for { // FIXME
      index <- (commits.indexOf(target) match {
        case -1 => Left("Target " + target.getName + " not included in current list")
        case i => Right(i)
      }).right
      orphans <- Right(commits.take(index)).right
      _ <- Right(repository.resetHard(target)).right
      newHead <- Right(repository.amendMessage(message)).right
      newLast <- finishUpdate(applyCommits(newHead, orphans.reverse)).right
    } yield newLast
  }

  def selectRange(rows: Seq[Int]) = {
    val selected = rows.map(i => this.commits(i)).toList
    new GraphRange(this, selected)
  }

  def contains(range: GraphRange): Boolean = range.graph == this

  def reorder(range: GraphRange, insertTo: Int): OperationResult = {
    if (range.isEmpty) return Right(this)

    val newOrder = commits.take(insertTo).filterNot(range.commits contains _) ++
      range.commits ++
      commits.drop(insertTo).filterNot(range.commits contains _)

    val (common, todo) = skipCommonRoot(newOrder.reverse, commits.reverse, startCommit)
    repository.resetHard(common)
    finishUpdate(applyCommits(common, todo))
  }

  def squash(range: GraphRange, newMessage: Option[String]): OperationResult = {
    if(range.commits.size <= 1) return Right(this)
    if(!isSequentialSlice(range.commits))
      return Left("Only sequential commits can be squashed.\nReorder commits before squashing")

    val squashLast: Commit = range.commits.head
    val orphans = commits.takeWhile(_ != squashLast)
    val message = newMessage.getOrElse(range.squashMessage)

    repository.resetHard(squashLast)
    repository.resetSoft(parent(range.commits.last))
    val newHead = repository.commit(message)
    finishUpdate(applyCommits(newHead, orphans.reverse))
  }

  def delete(range: GraphRange): OperationResult = {
    if (range.isEmpty) return Right(this)

    val newCommits = commits.filterNot(range.commits contains _)
    val (common, todo) = skipCommonRoot(newCommits.reverse, commits.reverse, startCommit)
    repository.resetHard(common)
    finishUpdate(applyCommits(common, todo))
  }

  def startEdit(commit: Commit): GraphRange = {
    val orphans = commits.takeWhile(_ != commit)
    repository.resetHard(commit)
    repository.resetSoft(parent(commit))
    new GraphRange(this, orphans)
  }

  def applyInteractively(range: GraphRange): Either[GraphRange, ArrangingGraph] = {
    if (!repository.isClean)
      return Left(range)
    @tailrec
    def loop(commits: List[Commit]): Either[GraphRange, ArrangingGraph] = commits match {
      case Nil => Right(new ArrangingGraph(repository, start, repository.resolve(Constants.HEAD).get))
      case x :: xs =>
        repository.cherryPick(x) match {
          case Left(err) => Left(new GraphRange(this, xs.reverse))
          case Right(_) => loop(xs)
        }
    }
    loop(range.commits.reverse)
  }

  private def isSequentialSlice(part: List[Commit]): Boolean = commits.containsSlice(part)

  // parent in the target graph
  private def parent(commit: Commit): Commit = {
    val index = commits.indexOf(commit)
    if(index != -1 && commits.indices.contains(index + 1))
      commits(index+1)
    else
      startCommit
  }

  // commits should be ordered from old to new
  private def applyCommits(first: Commit, commits: List[Commit]): Either[CherryPickFailure, Commit] = {
    commits.foldLeft[Either[CherryPickFailure, Commit]](Right(first))(
      (prev, c) => prev.right.flatMap(_ => repository.cherryPick(c).right.map[Commit](rev => rev)))
  }

  private def finishUpdate(newLast: Either[CherryPickFailure, Commit]): OperationResult = {
    newLast match {
      case Left(CherryPickFailure(commit)) =>
        rollback()
        Left("Cherry-pick failed at " + commit.getName)
      case Right(c) =>
        Right(new ArrangingGraph(repository, start, c))
    }
  }

  @tailrec
  private[this] def skipCommonRoot[A](xs: List[A], ys: List[A], common: A): (A, List[A]) = (xs, ys) match {
    case (Nil, _) => (common, Nil)
    case (x::xss, Nil) => (common, x::xss)
    case (x::xss, y::yss) => if (x == y) skipCommonRoot(xss, yss, x) else (common, x::xss)
  }
}

class GraphRange(val graph: ArrangingGraph, val commits: List[Commit]) {
  def squash(newMessage: Option[String]): graph.OperationResult = {
    graph squash(this, newMessage)
  }

  def delete(): graph.OperationResult = {
    graph.delete(this)
  }

  def applyInteractively: Either[GraphRange, ArrangingGraph] =
    graph.applyInteractively(this)

  def isEmpty: Boolean = commits.isEmpty

  def first: Option[Commit] = {
    commits.headOption
  }

  def squashMessage: String= {
    commits.reverse.map(_.getFullMessage.stripLineEnd).mkString("\n\n")
  }
}
