package com.tomykaira.uchronie.git

import com.tomykaira.uchronie.GitSpecHelper
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{FunSpec, BeforeAndAfter}
import com.tomykaira.uchronie.git.VirtualCommit.{Pick, Rename, DummyCommit}
import org.scalatest.EitherValues._

class CommitThreadSpec extends FunSpec with BeforeAndAfter with ShouldMatchers with GitSpecHelper {
  before {
    initRepo()
  }

  def vir(id: Int) = DummyCommit(id)
  lazy val commits = (1 to 10).map(DummyCommit).reverse.toList
  lazy val thread = CommitThread.fromVirtualCommits(commits)

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
    }
  }
}
