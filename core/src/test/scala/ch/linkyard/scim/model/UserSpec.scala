package ch.linkyard.scim.model

import io.circe.Json
import org.scalatest.OptionValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import ch.linkyard.scim.model.User.UserRef
import ch.linkyard.scim.model.Codecs.given

import java.net.URI
import ch.linkyard.scim.model.User.ValueWithTypeAndDisplay

class UserSpec extends AnyFunSpec with Matchers with OptionValues {

  describe("User") {
    it("should contain id") {
      User(Jsons.userMinimal).id.value should be("2819c223-7f76-453a-919d-413861904646")
    }

    it("should contain schema") {
      User(Jsons.userMinimal).schema should be(Schema("urn:ietf:params:scim:schemas:core:2.0:User"))
    }

    it("should serialize model with schema") {
      val json = User(User.Root(
        id = Some("2819c223-7f76-453a-919d-413861904646"),
        userName = "bjensen@example.com",
      )).json.noSpaces
      json should include("urn:ietf:params:scim:schemas:core:2.0:User")
    }

    it("should serialize primary email") {
      val json = User(User.Root(
        id = Some("2819c223-7f76-453a-919d-413861904646"),
        userName = "bjensen@example.com",
        emails = Some(List(ValueWithTypeAndDisplay(Some("bjensen@example.com"), None, None, Some(true))))
      )).json.noSpaces
      json should include("""{"value":"bjensen@example.com","primary":true}""")
    }

    it("should serialize multiple emails with type") {
      val json = User(User.Root(
        id = Some("2819c223-7f76-453a-919d-413861904646"),
        userName = "bjensen@example.com",
        emails = Some(List(
          ValueWithTypeAndDisplay(Some("bjensen@example.com"), Some("work"), None, Some(true)),
          ValueWithTypeAndDisplay(Some("bjensen@private.com"), Some("home"), None),
          ))
      )).json.noSpaces
      json should include("""{"value":"bjensen@example.com","display":"work","primary":true}""")
      json should include("""{"value":"bjensen@private.com","display":"home"}""")
    }

    it("should parse 'userMinimal'") {
      val root = User(Jsons.userMinimal).rootOrDefault
      root.userName should be("bjensen@example.com")
      root.id.value should be("2819c223-7f76-453a-919d-413861904646")
      root.name should be(None)
    }

    it("should parse 'userMinimalExternal'") {
      val root = User(Jsons.userMinimalExternal).rootOrDefault
      root.userName should be("bjensen@example.com")
      root.id should be(None)
      root.name should be(None)
    }

    it("should parse 'userFull'") {
      val root = User(Jsons.userFull).rootOrDefault
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
      val root = User(Jsons.userFull).rootOrDefault
      val enterprise = root.enterprise.value
      enterprise.employeeNumber.value should be("701984")
      enterprise.costCenter.value should be("4130")
      enterprise.organization.value should be("Universal Studios")
      enterprise.division.value should be("Theme Park")
      enterprise.department.value should be("Tour Operations")
      enterprise.manager.value should be(UserRef(
        value = "26118915-6090-4610-87e4-49d8ca9f808d",
        $ref = Some("../Users/26118915-6090-4610-87e4-49d8ca9f808d"),
        displayName = Some("John Smith"),
      ))
    }

    it("should serialize roundtrip to json without changing") {
      def ignoreMeta(json: Json): Json = json.mapObject(_.remove("meta"))
      def roundtrip(original: Json) = {
        val r = original.as[User]
        r.isRight should be(true)
        val user = r.getOrElse(fail(""))
        val json = user.asJson(URI.create("urn:none"))
        ignoreMeta(json) should be(ignoreMeta(original))
      }

      roundtrip(Jsons.userMinimal)
      roundtrip(Jsons.userFull)
    }
  }

}
