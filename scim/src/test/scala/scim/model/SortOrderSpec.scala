package scim.model

import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.matchers.should.Matchers

class SortOrderSpec extends AnyFunSpec with Checkers with Matchers {
  describe("SortOrder") {
    it("should asString and parse should be compatible")(check { order: SortOrder =>
      val string = order.asString
      SortOrder.parse(string) == Right(order)
    })

    describe("parse") {}
    it("should be default for empty") {
      SortOrder.parse("") should be(Right(SortOrder.default))
      SortOrder.parse(" ") should be(Right(SortOrder.default))
    }

    it("should be left from invalid") {
      SortOrder.parse("bla").isLeft should be(true)
      SortOrder.parse("hube").isLeft should be(true)
    }
  }
}
