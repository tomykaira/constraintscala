package com.tomykaira.uchronie.git

import com.tomykaira.uchronie.{TargetRange, GitSpecHelper}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{FunSpec, BeforeAndAfter}
import com.tomykaira.uchronie.git.Commit._
import com.tomykaira.uchronie.git.Operation._
import org.scalatest.EitherValues._

class CommitThreadSpec extends FunSpec with BeforeAndAfter with ShouldMatchers with GitSpecHelper {
  lazy val commits = (1 to 10).map(DummyCommit).reverse.toList
  lazy val thread = CommitThread.fromCommits(commits)
  lazy val notFound = DummyCommit(-1)

  describe("Initialize CommitThread from dummy commits") {
    it("should return Thread with commits") {
      thread.commits should have length 10
    }
  }

  describe("composite new thread") {
    describe("rename") {
      lazy val result = thread.applyOperation(RenameOp(5, "New Message"))
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
      it("should ignore operation if commit does not found") {
        val result = thread.applyOperation(RenameOp(-1, "New Message"))
        result should equal (Right(thread))
      }
    }
    describe("move") {
      lazy val result = thread.applyOperation(MoveOp(TargetRange(4, 6), 0))
      it("should move 3 commits to top") {
        val first = result.right.value.commits(0)
        val third = result.right.value.commits(2)
        first.asInstanceOf[Pick].previous should equal (commits(4))
        third.asInstanceOf[Pick].previous should equal (commits(6))
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
      it("should ignore operation if one of commits does not found") {
        val result = thread.applyOperation(MoveOp(TargetRange(4, 100), 0))
        result should equal (Right(thread))
      }
    }
    describe("squash") {
      lazy val result = thread.applyOperation(SquashOp(TargetRange(4, 6), None))
      it("should squash commits there") {
        val fifth = result.right.value.commits(4)
        fifth.asInstanceOf[Commit.Squash].previous.list should equal (commits.slice(4,7))
      }
      it("should set message by concatenating") {
        val fifth = result.right.value.commits(4)
        fifth.asInstanceOf[Commit.Squash].message should equal ("Dummy 4\n\nDummy 5\n\nDummy 6")
      }
      it("should set message from Operation") {
        val result = thread.applyOperation(SquashOp(TargetRange(4, 6), Some("New Message")))
        val fifth = result.right.value.commits(4)
        fifth.asInstanceOf[Commit.Squash].message should equal ("New Message")
      }
      it("should shorten thread length") {
        result.right.value.commits should have length 8
      }
      it("should pick 0 to 3") {
        val first = result.right.value.commits(0)
        first.asInstanceOf[Pick].previous should equal (commits(0))
      }
      it("should keep 7 to 9") {
        val eighth = result.right.value.commits(5)
        eighth should equal (commits(7))
      }
    }
    describe("delete") {
      lazy val result = thread.applyOperation(DeleteOp(5))
      it("should shorten thread length") {
        result.right.value.commits should have length 9
      }
      it("should pick 0 to 4") {
        val fifth = result.right.value.commits(4)
        fifth.asInstanceOf[Pick].previous should equal (commits(4))
      }
      it("should keep 6 to 9") {
        val sixth = result.right.value.commits(5)
        sixth should equal (commits(6))
      }
      it("should ignore operation if commit is not included") {
        val result = thread.applyOperation(DeleteOp(-1))
        result should equal (Right(thread))
      }
    }
  }

  describe("simplification") {
    it("should simplify rename + rename") {
      val result1 = thread.applyOperation(RenameOp(5, "New Message")).right.value
      val result2 = result1.applyOperation(RenameOp(5, "Next New Message")).right.value
      result2.commits(5) should equal (Rename(commits(5), "Next New Message"))
    }
  }

  describe("perform") {
    before {
      initRepo()
    }

    it("should take first not changed commits") {
      val commits = (1 to 10).map { i => createCommit(s"$i.txt", i.toString, i.toString)}.reverse.toList
      repository.resetHard(commits(9))
      val thread = CommitThread.fromCommits(commits.dropRight(1))
      val result = for {
        t <- thread.applyOperation(RenameOp(5, "New")).right        // 10 9 8 7 6 New 4 3 2
        r <- t.perform(repository).right
      } yield r
      val newCommits = result.right.value.commits
      newCommits(5).message should equal ("New")
      newCommits should have length 9
    }

    it("should perform operations") {
      val commits = (1 to 10).map { i => createCommit(s"$i.txt", i.toString, i.toString)}.reverse.toList
      repository.resetHard(commits(9))
      val thread = CommitThread.fromCommits(commits.dropRight(1))
      val result = for {
        t <- thread.applyOperation(RenameOp(5, "New")).right        // 10 9 8 7 6 New 4 3 2
        t <- t.applyOperation(MoveOp(TargetRange(5, 8), 0)).right         // New 4 3 2 10 9 8 7 6
        t <- t.applyOperation(DeleteOp(2)).right                  // New 4 2 10 9 8 7 6
        t <- t.applyOperation(SquashOp(TargetRange(1, 3), None)).right    // New 4-2-10 9 8 7 6
        r <- t.perform(repository).right
      } yield r
      result.right.value.commits.map(_.message) should equal (List("New", "10\n\n2\n\n4", "9", "8", "7", "6"))
    }
  }
}
