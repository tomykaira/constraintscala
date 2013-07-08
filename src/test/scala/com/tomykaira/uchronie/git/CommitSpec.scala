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

    describe("Pick") {
      it("should do git pick") {
        val commits = List(
          createCommit("A", "1st", "1st"),
          createCommit("B", "2nd", "2nd"),
          createCommit("C", "3rd", "3rd"))
        repository.resetHard(commits(0))
        val newCommit = Pick(commits(2)).perform(repository).right.value
        newCommit.message should equal ("3rd")
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
        val commits = List(
          createCommit("A", "1st", "1st"),
          createCommit("A", "2nd", "2nd"),
          createCommit("A", "3rd", "3rd"))
        repository.resetHard(commits(0))
        val result = Pick(Rename(commits(2), "Foo")).perform(repository)
        result should be ('left)
        assert(result.left.value.isInstanceOf[Commit.NotSimple])
      }
    }
  }
}
