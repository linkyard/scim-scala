package scim.model

import java.net.URI
import io.circe.syntax._
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.OptionValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers
import scim.model.Codecs._
import Arbitraries._
import org.scalacheck.ScalacheckShapeless._

class ServiceProviderConfigurationSpec extends AnyFunSpec with Checkers with Matchers with OptionValues {
  describe("ServiceProviderConfiguration") {
    it("should serialize/parse to same Json")(check { config: ServiceProviderConfiguration =>
      val json = config.asJson(URI.create("urn:none"))
      val r = json.as[ServiceProviderConfiguration]
      val reparsed = r.getOrElse(fail(s"could not parse: ${r}"))
      reparsed == config
    })
  }
}
