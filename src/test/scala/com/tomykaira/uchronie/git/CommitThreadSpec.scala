package com.tomykaira.uchronie.git

import com.tomykaira.uchronie.GitSpecHelper
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{FunSpec, BeforeAndAfter}
import com.tomykaira.uchronie.git.VirtualCommit.{Move, Pick, Rename, DummyCommit}
import org.scalatest.EitherValues._

class CommitThreadSpec extends FunSpec with BeforeAndAfter with ShouldMatchers with GitSpecHelper {
  before {
    initRepo()
  }

  lazy val commits = (1 to 10).map(DummyCommit).reverse.toList
  lazy val thread = CommitThread.fromVirtualCommits(commits)
  lazy val notFound = DummyCommit(-1)

  describe("Initialize CommitThread from dummy commits") {
    it("should return Thread with commits") {
      thread.commits should have length 10
    }
  }

  describe("composite new thread") {
    describe("rename") {
      lazy val result = thread.applyOperation(RenameOp(commits(5), "New Message"))
      it("should mark the renamed commit") {
        val fifth = result.right.value.commits(5)
        assert(fifth.isInstanceOf[Rename])
        fifth.asInstanceOf[Rename].previous should equal (commits(5))
        fifth.asInstanceOf[Rename].message should equal ("New Message")
      }
      it("should keep the size") {
        result.right.value.commits should have length 10
      }
      it("should use the same commits before renamed") {
        result.right.value.commits(6) should equal (commits(6))
      }
      it("should make commits after renamed Pick") {
        val after = result.right.value.commits(4)
        assert(after.isInstanceOf[Pick])
        after.asInstanceOf[Pick].previous should equal (commits(4))
      }
      it("should fail if commit does not found") {
        val result = thread.applyOperation(RenameOp(notFound, "New Message"))
        result should be ('left)
        result.left.value.asInstanceOf[CommitThread.CommitNotFound].commit should equal (notFound)
      }
    }
    describe("move") {
      lazy val result = thread.applyOperation(MoveOp(commits.slice(4,7), 0))
      it("should move 3 commits to top") {
        val first = result.right.value.commits(0)
        val third = result.right.value.commits(2)
        first.asInstanceOf[Move].previous should equal (commits(4))
        third.asInstanceOf[Move].previous should equal (commits(6))
      }
      it("should pick 0 to 3") {
        val fourth = result.right.value.commits(3)
        val seventh = result.right.value.commits(6)
        fourth.asInstanceOf[Pick].previous should equal (commits(0))
        seventh.asInstanceOf[Pick].previous should equal (commits(3))
      }
      it("should keep 7 to 9") {
        val eighth = result.right.value.commits(7)
        eighth should equal (commits(7))
      }
      it("should fail if one of commits does not found") {
        val result = thread.applyOperation(MoveOp(notFound :: commits.slice(4,7), 0))
        result should be ('left)
        result.left.value.asInstanceOf[CommitThread.CommitNotFound].commit should equal (notFound)
      }
    }
  }
}
