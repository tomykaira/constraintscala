package io.github.tomykaira.constraintscala

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class StaticConstraintSpec extends FunSpec with ShouldMatchers {
  trait Fixture {
    val constraint = new StaticConstraint[Int](3)
    var called = false
    def checkerCallback(cond: Int => Boolean) = { given: Int => called = cond(given) }
  }

  describe("get") {
    it("should return initial value") {
      new Fixture {
        constraint.get should equal (3)
      }
    }

    it("should return the new value after update") {
      new Fixture {
        constraint.update(5)
        constraint.get should equal(5)
      }
    }
  }

  describe("callback") {
    it("should invoke callback on registering") {
      new Fixture {
        constraint.onChange(checkerCallback(_ == 3))
        assert(called)
      }
    }

    it("should invoke callback on update") {
      new Fixture {
        constraint.onChange(checkerCallback(_ == 5))
        assert(!called)
        constraint.update(5)
        assert(called)
      }
    }

    it("should invoke chain on update") {
      new Fixture {
        val plus1Constraint = new Constraint[Int]({constraint.get + 1})
        plus1Constraint.onChange(checkerCallback(_ == 6))
        assert(!called)
        constraint.update(5)
        assert(called)
      }
    }
  }
}
