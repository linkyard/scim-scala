package ch.linkyard.scim.model

import io.circe.Json
import org.scalatest.OptionValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import ch.linkyard.scim.model.Codecs.given

import java.net.URI

class GroupSpec extends AnyFunSpec with Matchers with OptionValues {

  describe("Group") {
    it("should contain id") {
      Group(Jsons.group).id.value should be("6c5bb468-14b2-4183-baf2-06d523e03bd3")
    }

    it("should contain schema") {
      Group(Jsons.group).schema should be(Schema("urn:ietf:params:scim:schemas:core:2.0:Group"))
    }


    it("should serialize model with schema") {
      val json = Group(Group.Root(
        id = Some("6c5bb468-14b2-4183-baf2-06d523e03bd3"),
        displayName = "Group B",
      )).json.noSpaces
      json should include("urn:ietf:params:scim:schemas:core:2.0:Group")
    }

    it("should parse a the group json from the spec") {
      val root = Group(Jsons.group).rootOrDefault
      root.id.value should be("6c5bb468-14b2-4183-baf2-06d523e03bd3")
      root.displayName should be("Group B")
      root.members.value should have size 1
    }

    it("should parse the members of the group json from the spec") {
      val root = Group(Jsons.group).rootOrDefault
      root.members.value should have size 1
      val m0 = root.members.value.head
      m0.value should be("c3a26dd3-27a0-4dec-a2ac-ce211e105f97")
      m0.`$ref`.value should be("https://example.com/v2/Groups/c3a26dd3-27a0-4dec-a2ac-ce211e105f97")
      m0.display should be(None)
    }

    it("should serialize roundtrip to json without changing") {
      def ignoreMeta(json: Json): Json = json.mapObject(_.remove("meta"))
      def roundtrip(original: Json) = {
        val r = original.as[Group]
        r.isRight should be(true)
        val group = r.getOrElse(fail(""))
        val json = group.asJson(URI.create("urn:none"))
        ignoreMeta(json) should be(ignoreMeta(original))
      }

      roundtrip(Jsons.group)
      roundtrip(Jsons.group2)
    }
  }
}
