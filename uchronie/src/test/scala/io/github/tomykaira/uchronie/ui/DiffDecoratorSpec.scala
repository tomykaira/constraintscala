package io.github.tomykaira.uchronie.ui

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{FunSpec, BeforeAndAfter}
import io.github.tomykaira.uchronie.GitSpecHelper

class DiffDecoratorSpec extends FunSpec with BeforeAndAfter with ShouldMatchers with GitSpecHelper {
  before {
    initRepo()
  }

  describe("All") {
    it("should take prefix") {
      DiffDecorator.All("1295235", List()).name should equal ("ALL of 1295235")
    }
    it("should include diff of all files") {
      firstCommit
      addFile("test", "goodbye")
      addFile("test2", "Hello2")
      val commit = doCommit("Two files")
      val diff = DiffDecorator.All("", repository.diff(commit)).fullDiff(repository)
      diff should include ("a/test")
      diff should include ("b/test")
      diff should include ("b/test2")
    }
  }
}
