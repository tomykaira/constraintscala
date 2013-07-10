package com.tomykaira.uchronie.ui

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{FunSpec, BeforeAndAfter}
import com.tomykaira.uchronie.GitSpecHelper

class DiffDecoratorSpec extends FunSpec with BeforeAndAfter with ShouldMatchers with GitSpecHelper {
  before {
    initRepo()
  }

  describe("All") {
    it("should named All") {
      DiffDecorator.All(None, List()).prefixedName should equal ("All")
    }
    it("should take prefix") {
      DiffDecorator.All(Some("1295235"), List()).prefixedName should equal ("1295235: All")
    }
    it("should include diff of all files") {
      firstCommit
      addFile("test", "goodbye")
      addFile("test2", "Hello2")
      val commit = doCommit("Two files")
      val diff = DiffDecorator.All(None, repository.diff(commit)).fullDiff(repository)
      diff should include ("a/test")
      diff should include ("b/test")
      diff should include ("b/test2")
    }
  }
}
