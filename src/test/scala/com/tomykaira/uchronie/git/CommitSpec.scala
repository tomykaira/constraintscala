package com.tomykaira.uchronie.git

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{BeforeAndAfter, FunSpec}
import com.tomykaira.uchronie.git.Commit._
import com.tomykaira.uchronie.GitSpecHelper
import org.scalatest.EitherValues._
import scalaz.NonEmptyList

class CommitSpec extends FunSpec with BeforeAndAfter with ShouldMatchers with GitSpecHelper {

  def createSquash(cs: List[Commit], message: String): Commit.Squash =
    Commit.Squash(NonEmptyList.nel(cs.head, cs.tail), message)

  def createSquash(ids: Range, message: String): Commit.Squash =
    createSquash(ids.toList.map(dummyCommit), message)

  describe("perform") {
    before {
      initRepo()
    }

    trait Fixture {
      val commits = List(
        createCommit("A", "1st", "1st"),
        createCommit("B", "2nd", "2nd"),
        createCommit("C", "3rd", "3rd"),
        createCommit("D", "4rd", "4rd"))
    }

    describe("Pick") {
      it("should do git pick") {
        new Fixture {
          repository.resetHard(commits(0))
          val newCommit = Pick(commits(2)).perform(repository).right.value
          newCommit.message should equal ("3rd")
        }
      }
      it("should return error if pick failed") {
        val commits = List(
          createCommit("A", "1st", "1st"),
          createCommit("A", "2nd", "2nd"),
          createCommit("A", "3rd", "3rd"))
        repository.resetHard(commits(0))
        val result = Pick(commits(2)).perform(repository)
        result should be ('left)
      }
    }

    describe("Rename") {
      it("should rename a new commit") {
        new Fixture {
          repository.resetHard(commits(0))
          val result = Rename(commits(2), "Foo").perform(repository)
          result.right.value.message should equal ("Foo")
        }
      }
    }

    describe("Squash") {
      it("should squash commits") {
        new Fixture {
          repository.resetHard(commits(0))
          val commit = createSquash(commits.slice(1,4).reverse, "Foo").perform(repository).right.value
          commit.message should equal ("Foo")
          commit.raw.getParent(0) should equal (commits(0).getId)
        }
      }
      it("should squash not sequential commits") {
        new Fixture {
          repository.resetHard(commits(0))
          val commit = createSquash(List(commits(3), commits(1)), "Foo").perform(repository).right.value
          commit.message should equal ("Foo")
          commit.raw.getParent(0) should equal (commits(0).getId)
        }
      }
      it("should squash edits on the same file commits") {
        val commits = List(
          createCommit("A", "1st", "1st"),
          createCommit("A", "2nd", "2nd"),
          createCommit("A", "3rd", "3rd"),
          createCommit("A", "4rd", "4rd"))
        repository.resetHard(commits(0))
        val commit = createSquash(commits.slice(1,4).reverse, "Foo").perform(repository).right.value
        commit.message should equal ("Foo")
      }
    }
  }
}
