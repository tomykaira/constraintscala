package com.tomykaira.uchronie.git

import com.tomykaira.uchronie.GitSpecHelper
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{FunSpec, BeforeAndAfter}
import com.tomykaira.uchronie.git.VirtualCommit.DummyCommit

class CommitThreadSpec extends FunSpec with BeforeAndAfter with ShouldMatchers with GitSpecHelper {
  before {
    initRepo()
  }

  def vir(id: Int) = DummyCommit(id)

  describe("Initialize CommitThread from dummy commits") {
    it("should return Thread with commits") {
      val thread = CommitThread.fromVirtualCommits(List(vir(3), vir(2), vir(1)))
      thread.commits should have length 3
    }
  }
}
