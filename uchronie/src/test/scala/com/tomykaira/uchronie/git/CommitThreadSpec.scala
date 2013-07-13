package com.tomykaira.uchronie.git

import com.tomykaira.uchronie.{TargetRange, GitSpecHelper}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{FunSpec, BeforeAndAfter}
import com.tomykaira.uchronie.git.Commit._
import com.tomykaira.uchronie.git.Operation._
import org.scalatest.EitherValues._

class CommitThreadSpec extends FunSpec with BeforeAndAfter with ShouldMatchers with GitSpecHelper {
  lazy val commits = (1 to 10).map(dummyCommit).reverse.toList
  lazy val thread = CommitThread.fromCommits(commits)

  describe("composite new thread") {
    describe("rename") {
      lazy val result = thread.applyOperation(RenameOp(5, "New Message"))
      it("should mark the renamed commit") {
        val fifth = result.commits(5)
        assert(fifth.isInstanceOf[Rename])
        fifth.asInstanceOf[Rename].previous should equal (commits(5))
        fifth.asInstanceOf[Rename].message should equal ("New Message")
      }
      it("should keep the size") {
        result.commits should have length 10
      }
      it("should use the same commits before renamed") {
        result.commits(6) should equal (commits(6))
      }
      it("should make commits after renamed Pick") {
        val after = result.commits(4)
        assert(after.isInstanceOf[Pick])
        after.asInstanceOf[Pick].previous should equal (commits(4))
      }
      it("should ignore operation if commit does not found") {
        val result = thread.applyOperation(RenameOp(-1, "New Message"))
        result should equal (thread)
      }
    }
    describe("move") {
      lazy val result = thread.applyOperation(MoveOp(TargetRange(4, 6), 0))
      it("should move 3 commits to top") {
        val first = result.commits(0)
        val third = result.commits(2)
        first.asInstanceOf[Pick].previous should equal (commits(4))
        third.asInstanceOf[Pick].previous should equal (commits(6))
      }
      it("should pick 0 to 3") {
        val fourth = result.commits(3)
        val seventh = result.commits(6)
        fourth.asInstanceOf[Pick].previous should equal (commits(0))
        seventh.asInstanceOf[Pick].previous should equal (commits(3))
      }
      it("should keep 7 to 9") {
        val eighth = result.commits(7)
        eighth should equal (commits(7))
      }
      it("should ignore operation if one of commits does not found") {
        val result = thread.applyOperation(MoveOp(TargetRange(4, 100), 0))
        result should equal (thread)
      }
      it("should pick after commits when moving downward") {
        // 10 9 8 7 3 _6 5 4_ 2 1
        val result = thread.applyOperation(MoveOp(TargetRange(4, 6), 8))
        assert(result.commits.slice(0, 6).forall(_.isInstanceOf[Pick]))
        assert(result.commits.slice(6, 8).forall(_.isInstanceOf[Pick]))
        assert(result.commits.slice(8, 10).forall(_.isInstanceOf[Raw]))
      }
      it("should ignore if all commits are in the same position") {
        val result = thread.applyOperation(MoveOp(TargetRange(4, 7), 6))
        result should equal (thread)
      }
    }
    describe("squash") {
      lazy val result = thread.applyOperation(SquashOp(TargetRange(4, 6), None))
      it("should squash commits there") {
        val fifth = result.commits(4)
        fifth.asInstanceOf[Commit.Squash].previous should equal (commits.slice(4,7))
      }
      it("should set message by concatenating") {
        val fifth = result.commits(4)
        fifth.asInstanceOf[Commit.Squash].message should equal ("Dummy 4\n\nDummy 5\n\nDummy 6")
      }
      it("should set message from Operation") {
        val result = thread.applyOperation(SquashOp(TargetRange(4, 6), Some("New Message")))
        val fifth = result.commits(4)
        fifth.asInstanceOf[Commit.Squash].message should equal ("New Message")
      }
      it("should shorten thread length") {
        result.commits should have length 8
      }
      it("should pick 0 to 3") {
        val first = result.commits(0)
        first.asInstanceOf[Pick].previous should equal (commits(0))
      }
      it("should keep 7 to 9") {
        val eighth = result.commits(5)
        eighth should equal (commits(7))
      }
    }
    describe("delete") {
      lazy val result = thread.applyOperation(DeleteOp(5))
      it("should shorten thread length") {
        result.commits should have length 9
      }
      it("should pick 0 to 4") {
        val fifth = result.commits(4)
        fifth.asInstanceOf[Pick].previous should equal (commits(4))
      }
      it("should keep 6 to 9") {
        val sixth = result.commits(5)
        sixth should equal (commits(6))
      }
      it("should ignore operation if commit is not included") {
        val result = thread.applyOperation(DeleteOp(-1))
        result should equal (thread)
      }
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

      // 10 9 8 7 6 New 4 3 2
      val result = thread.applyOperation(RenameOp(5, "New")).perform(repository)

      val newCommits = result.right.value
      newCommits(5).message should equal ("New")
      newCommits should have length 9
    }

    it("should return empty list if every commits are deleted") {
      CommitThread.fromCommits(List()).perform(repository) should equal (Right(List()))
    }

    it("should perform operations") {
      val commits = (1 to 10).map { i => createCommit(s"$i.txt", i.toString, i.toString)}.reverse.toList
      repository.resetHard(commits(9))
      val thread = CommitThread.fromCommits(commits.dropRight(1))

      val result = thread.applyOperation(RenameOp(5, "New")). // 10 9 8 7 6 New 4 3 2
        applyOperation(MoveOp(TargetRange(5, 8), 0)).         // New 4 3 2 10 9 8 7 6
        applyOperation(DeleteOp(2)).                          // New 4 2 10 9 8 7 6
        applyOperation(SquashOp(TargetRange(1, 3), None)).    // New 4-2-10 9 8 7 6
        perform(repository)

      result.right.value.map(_.message) should equal (List("New", "10\n\n2\n\n4", "9", "8", "7", "6"))
    }
  }
}
