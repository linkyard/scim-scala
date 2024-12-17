package ch.linkyard.scim.model

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers
import ch.linkyard.scim.model.Arbitraries.given

class SortOrderSpec extends AnyFunSpec with Checkers with Matchers {
  describe("SortOrder") {
    describe("parse") {
      it("and asString should be compatible")(check { (order: SortOrder) =>
        val string = order.asString
        SortOrder.parse(string) == Right(order)
      })

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
}
