package com.tomykaira.uchronie

import org.scalatest.{BeforeAndAfter, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import com.tomykaira.uchronie.CommitDecorator

class CommitDecoratorSpec extends FunSpec with BeforeAndAfter with ShouldMatchers with GitSpecHelper {
  before {
    initRepo()
  }

  describe("tableRow") {
    it("should return a row of short id and short comment") {
      val rawCommit = createCommit("test", "content", "Comment head\n\nLong description")
      val commit = new CommitDecorator(rawCommit)
      val row = commit.tableRow(repository)
      row(0) should startWith (rawCommit.getId.name.substring(0, 7))
      row(1) should equal ("Comment head")
    }
  }
}