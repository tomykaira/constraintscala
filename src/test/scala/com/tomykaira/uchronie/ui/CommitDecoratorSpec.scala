package com.tomykaira.uchronie.ui

import com.tomykaira.uchronie.GitSpecHelper
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{BeforeAndAfter, FunSpec}

class CommitDecoratorSpec extends FunSpec with BeforeAndAfter with ShouldMatchers with GitSpecHelper  {
  before {
    initRepo()
  }
  describe("oneLineMessage") {
    it("should show the whole if oneline") {
      val commit = createCommit("commit message")
      new CommitDecorator(commit).oneLineMessage should equal ("commit message")
    }
    it("should show the first line if multi line") {
      val commit = createCommit("commit summary\n\nthis is foo bar.")
      new CommitDecorator(commit).oneLineMessage should equal ("commit summary")
    }
  }
}
