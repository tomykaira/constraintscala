package com.tomykaira.uchronie.git

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSpec
import com.tomykaira.uchronie.git.VirtualCommit._

class CommitSpec extends FunSpec with ShouldMatchers {
  describe("simplify") {
    val core = DummyCommit(3)
    describe("Pick") {
      it("should simplify enclosing Pick") {
        Pick(Pick(core)).simplify should equal (Pick(core))
      }
      it("should simplify two enclosing Pick") {
        Pick(Pick(Pick(core))).simplify should equal (Pick(core))
      }
      it("should simplify Rename") {
        Pick(Rename(core, "A")).simplify should equal (Rename(core, "A"))
      }
    }
    describe("Rename") {
      it("should simplify sequential Rename") {
        Rename(Rename(core, "A"), "B").simplify should equal (Rename(core, "B"))
      }
      it("should omit Pick") {
        Rename(Pick(Pick(core)), "B").simplify should equal (Rename(core, "B"))
      }
    }
    describe("squash") {
      it("should simplify each commit") {
        VirtualCommit.Squash(List(Pick(Pick(core))), "Foo").simplify should equal (VirtualCommit.Squash(List(Pick(core)), "Foo"))
      }
    }
  }
}
