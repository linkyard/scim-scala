package scim.model

import org.scalatest.OptionValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers
import scim.model.Arbitraries.given
import scim.model.Codecs.given

import java.net.URI

class ResourceTypeSpec extends AnyFunSpec with Checkers with Matchers with OptionValues {
  describe("ResourceType") {
    it("should serialize/parse to same Json")(check { (rt: ResourceType) =>
      val json = rt.asJson(URI.create("urn:none"))
      val r = json.as[ResourceType]
      val reparsed = r.getOrElse(fail(s"could not parse: $r"))
      reparsed == rt
    })
  }
}
