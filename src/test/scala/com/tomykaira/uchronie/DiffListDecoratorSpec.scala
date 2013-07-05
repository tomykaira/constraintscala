package com.tomykaira.uchronie

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{FunSpec, BeforeAndAfter}

class DiffListDecoratorSpec extends FunSpec with BeforeAndAfter with ShouldMatchers with GitSpecHelper {
  before {
    initRepo()
  }

  describe("fullDiff") {
    it("should include diff of all files") {
      firstCommit
      addFile("test", "goodbye")
      addFile("test2", "Hello2")
      val commit = doCommit("Two files")
      val diff = new DiffListDecorator(repository.diff(commit)).fullDiff(repository)
      diff should include ("a/test")
      diff should include ("b/test")
      diff should include ("b/test2")
    }
  }
}
