package com.tomykaira.uchronie.git

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{BeforeAndAfter, FunSpec}
import com.tomykaira.uchronie.git.Commit._
import com.tomykaira.uchronie.GitSpecHelper
import org.scalatest.EitherValues._

class CommitSpec extends FunSpec with BeforeAndAfter with ShouldMatchers with GitSpecHelper {
  describe("simplify") {
    val core = DummyCommit(3)
    describe("Pick") {
      it("should simplify enclosing Pick") {
        Pick(Pick(core)).simplify should equal (Pick(core))
      }
      it("should simplify two enclosing Pick") {
        Pick(Pick(Pick(core))).simplify should equal (Pick(core))
      }
      it("should simplify Rename") {
        Pick(Rename(core, "A")).simplify should equal (Rename(core, "A"))
      }
    }
    describe("Rename") {
      it("should simplify sequential Rename") {
        Rename(Rename(core, "A"), "B").simplify should equal (Rename(core, "B"))
      }
      it("should omit Pick") {
        Rename(Pick(Pick(core)), "B").simplify should equal (Rename(core, "B"))
      }
    }
    describe("Squash") {
      it("should simplify each commit") {
        Commit.Squash(List(Pick(Pick(core))), "Foo").simplify should equal (Commit.Squash(List(Pick(core)), "Foo"))
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
        assert(result.left.value.isInstanceOf[Commit.Failed])
      }
      it("should not commit if previous is OperationCommit") {
        new Fixture {
          repository.resetHard(commits(0))
          val result = Pick(Rename(commits(2), "Foo")).perform(repository)
          result should be ('left)
          assert(result.left.value.isInstanceOf[Commit.NotSimple])
        }
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
          val commit = Commit.Squash(commits.slice(1,4).reverse, "Foo").perform(repository).right.value
          commit.message should equal ("Foo")
          commit.raw.getParent(0) should equal (commits(0).getId)
        }
      }
      it("should squash not sequential commits") {
        new Fixture {
          repository.resetHard(commits(0))
          val commit = Commit.Squash(List(commits(3), commits(1)), "Foo").perform(repository).right.value
          commit.message should equal ("Foo")
          commit.raw.getParent(0) should equal (commits(0).getId)
        }
      }
      it("should return error if commits is empty") {
        new Fixture {
          repository.resetHard(commits(0))
          val result = Commit.Squash(List(), "Foo").perform(repository)
          result should equal (Left(Commit.EmptySquash()))
        }
      }
    }
  }
}
