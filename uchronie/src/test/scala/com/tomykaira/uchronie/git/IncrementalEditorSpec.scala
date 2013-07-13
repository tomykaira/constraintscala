package com.tomykaira.uchronie.git

import com.tomykaira.uchronie.GitSpecHelper
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{FunSpec, BeforeAndAfter}
import org.eclipse.jgit.lib.Constants

class IncrementalEditorSpec extends FunSpec with BeforeAndAfter with ShouldMatchers with GitSpecHelper {
  before {
    initRepo()
  }

  trait Fixture {
    val commits = List(
      createCommit("A", "1st", "1st"),
      createCommit("B", "2nd", "2nd"),
      createCommit("C", "3rd", "3rd"),
      createCommit("D", "4th", "4th"))
  }

  trait ConflictFixture {
    val commits = List(
      createCommit("A", "1st", "1st"),
      createCommit("A", "2nd", "2nd"),
      createCommit("A", "3rd", "3rd"),
      createCommit("A", "4th", "4th"))
  }

  describe("startEdit") {
    it("should checkout the parent of specified commit") {
      new Fixture {
        IncrementalEditor.startEdit(repository, commits(1), commits.slice(2,4))
        repository.resolve(Constants.HEAD).get should equal (commits(1).getId)
      }
    }
    it("should have files indexed") {
      new Fixture {
        IncrementalEditor.startEdit(repository, commits(1), commits.slice(2,4))
        assert(!repository.isClean)
      }
    }
    it("should return orphan commits") {
      new Fixture {
        val going = IncrementalEditor.startEdit(repository, commits(1), commits.slice(2,4))
        going.commits should equal (List(commits(3)))
      }
    }
  }

  describe("applyInteractively") {
    it("should return new ArrangingGraph if success") {
      new Fixture {
        repository.resetHard(commits(1))
        val going = IncrementalEditor.Going(repository, commits.slice(3, 4), commits(1))
        val done = going.continue
        done.head.message should equal ("4th")
      }
    }
    it("should return succeeding commits if failure") {
      new ConflictFixture {
        repository.resetHard(commits(0))
        val going = IncrementalEditor.Going(repository, commits.slice(2, 4), commits(0))
        val rest = going.continue
        rest.commits should equal (List(commits(3)))
      }
    }
    it("should keep repository dirty") {
      new ConflictFixture {
        repository.resetHard(commits(0))
        val going = IncrementalEditor.Going(repository, commits.slice(2, 4), commits(1))
        going.continue
        assert(!repository.isClean)
      }
    }
    it("should return given range if already dirty") {
      new Fixture {
        repository.resetSoft(commits(0))
        val going = IncrementalEditor.Going(repository, commits.slice(2, 4), commits(1))
        going.continue should equal (going)
      }
    }
  }
}
