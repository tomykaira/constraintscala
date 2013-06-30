package com.tomykaira.uchronie

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{FunSpec, BeforeAndAfter}
import org.scalatest.EitherValues._

class ArrangingGraphSpec extends FunSpec with BeforeAndAfter with ShouldMatchers with GitSpecHelper {
  before {
    initRepo()
  }

  describe("updateComment") {
    it("should create new ArrangingGraph inheriting current") {
      val start = createCommit("1st")
      createCommit("2nd")
      val target = createCommit("3rd")
      val end = createCommit("4th")
      val original = repository.listCommits(start, end)
      original.commits should have length 3 // gauntlet
      val graph = original.updateComment(target, "New 3rd").right.value
      graph.commits.map(_.getFullMessage) should equal (List("4th", "New 3rd", "2nd"))
    }
  }
}
