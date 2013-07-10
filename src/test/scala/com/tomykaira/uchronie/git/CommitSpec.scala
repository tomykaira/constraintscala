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

  describe("simplify") {
    val core = dummyCommit(3)
    lazy val pick = Pick(dummyCommit(5))
    lazy val rename = Commit.Rename(dummyCommit(4), "Rename")
    lazy val squash = createSquash(1 to 3, "Squash")
    describe("Pick") {
      it("should simplify enclosing Pick") {
        Pick(pick).simplify should equal (pick)
      }
      it("should simplify two enclosing Pick") {
        Pick(Pick(pick)).simplify should equal (pick)
      }
      it("should simplify Rename") {
        Pick(rename).simplify should equal (rename)
      }
      it("should simplify Squash") {
        Pick(squash).simplify should equal (squash)
      }
    }
    describe("Rename") {
      it("should omit Pick") {
        Rename(Pick(Pick(core)), "B").simplify should equal (Rename(core, "B"))
      }
      it("should simplify sequential Rename") {
        Rename(rename, "B").simplify should equal (rename.copy(message = "B"))
      }
      it("should simplify squash -> rename to one squash") {
        Rename(squash, "B").simplify should equal (squash.copy(message = "B"))
      }
    }
    describe("Squash") {
      it("should omit Pick") {
        createSquash(List(Pick(Pick(core))), "Foo").simplify should equal (createSquash(List(core), "Foo"))
      }
      it("should ignore Rename") {
        createSquash(List(Rename(core, "A"), dummyCommit(2)), "Foo").simplify.
          should(equal (createSquash(List(core, dummyCommit(2)), "Foo")))
      }
      it("should expand internal squash") {
        createSquash(List(dummyCommit(9), squash, dummyCommit(8)), "Foo").simplify.
          should(equal (createSquash(List(9, 1, 2, 3, 8).map(dummyCommit), "Foo")))
      }
    }
  }

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
