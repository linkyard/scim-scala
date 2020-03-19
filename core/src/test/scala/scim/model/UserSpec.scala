package scim.model

import java.net.URI
import io.circe.Json
import org.scalatest.OptionValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import scim.model.User.UserRef

class UserSpec extends AnyFunSpec with Matchers with OptionValues {

  describe("User") {
    it("should contain id") {
      User(Jsons.userMinimal).id.value should be("2819c223-7f76-453a-919d-413861904646")
    }
    it("should contain schema") {
      User(Jsons.userMinimal).schema should be(Schema("urn:ietf:params:scim:schemas:core:2.0:User"))
    }

    it("should parse 'userMinimal'") {
      val root = User(Jsons.userMinimal).root
      root.userName should be("bjensen@example.com")
      root.id.value should be("2819c223-7f76-453a-919d-413861904646")
      root.name should be(None)
    }
    it("should parse 'userMinimalExternal'") {
      val root = User(Jsons.userMinimalExternal).root
      root.userName should be("bjensen@example.com")
      root.id should be(None)
      root.name should be(None)
    }

    it("should parse 'userFull'") {
      val root = User(Jsons.userFull).root
      root.id.value should be("2819c223-7f76-453a-919d-413861904646")
      root.userName should be("bjensen@example.com")
      root.name.get.formatted.value should be("Ms. Barbara J Jensen, III")
      root.name.get.familyName.value should be("Jensen")
      root.displayName.value should be("Babs Jensen")
      root.profileUrl.value should be(URI.create("https://login.example.com/bjensen"))
      root.emails.value should have size 2
      root.addresses.value should have size 2
      root.x509Certificates.value.head.value should be("MI..Mo=")
    }

    it("should parse enterprise attrs") {
      val root = User(Jsons.userFull).root
      val enterprise = root.enterprise.value
      enterprise.employeeNumber.value should be("701984")
      enterprise.costCenter.value should be("4130")
      enterprise.organization.value should be("Universal Studios")
      enterprise.division.value should be("Theme Park")
      enterprise.department.value should be("Tour Operations")
      enterprise.manager.value should be(UserRef(
        value = "26118915-6090-4610-87e4-49d8ca9f808d",
        $ref = Some("../Users/26118915-6090-4610-87e4-49d8ca9f808d"),
        displayName = Some("John Smith")))
    }

    it("should serialize roundtrip to json without changing") {
      def roundtrip(original: Json) = {
        val r = Codecs.userDecoder.decodeJson(original)
        r.isRight should be(true)
        val user = r.getOrElse(fail(""))
        val json = Codecs.extensibleModelEncoder(user)
        json should be(original)
      }

      roundtrip(Jsons.userMinimal)
      roundtrip(Jsons.userFull)
    }
  }

}
