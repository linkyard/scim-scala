package scim.model

import io.circe.syntax._
import io.circe.{Json, parser}
import Codecs._
import Arbitraries._
import org.scalatest.OptionValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers

class ResourceTypeSpec extends AnyFunSpec with Checkers with Matchers with OptionValues {
  describe("ResourceType") {
    it("should serialize/parse to same Json")(check { rt: ResourceType =>
      val json = rt.asJson
      val r = json.as[ResourceType]
      val reparsed = r.getOrElse(fail(s"could not parse: ${r}"))
      reparsed == rt
    })
  }
}
