package scim.model

import io.circe.Json
import io.circe.parser
import io.circe.syntax.*
import org.scalatest.OptionValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers
import scim.model.Arbitraries.given
import scim.model.Codecs.*

class PatchOpSpec extends AnyFunSpec with Checkers with Matchers with OptionValues {
  describe("PatchOp") {
    it("should serialize/parse to same Json")(check { (patch: PatchOp) =>
      val json = patch.asJson
      val r = json.as[PatchOp]
      val reparsed = r.getOrElse(fail(s"could not parse: $r"))
      reparsed == patch
    })

    describe("applyTo") {
      def parseJson(json: String): Json =
        parser.parse(json).fold(e => fail(s"unparsable json: ${e.message}"), identity)

      def patchOp(json: String): PatchOp =
        parser.parse(json).flatMap(_.as[PatchOp]).fold(e => fail(s"PatchOp not parsable: ${e.getMessage}"), identity)

      it("should add a member to a group (element to array)") {
        val patch = patchOp(
          """
            |   { "schemas":
            |      ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
            |     "Operations":[
            |       {
            |        "op":"add",
            |        "path":"members",
            |        "value":[
            |         {
            |           "display": "Babs Jensen",
            |           "$ref":
            |   "https://example.com/v2/Users/2819c223...413861904646",
            |           "value": "2819c223-7f76-453a-919d-413861904646"
            |         }
            |        ]
            |       }
            |     ]
            |   }
            |""".stripMargin
        )

        val before = Jsons.group

        val after = parseJson(
          """
            |       {
            |         "id": "6c5bb468-14b2-4183-baf2-06d523e03bd3",
            |         "schemas": ["urn:ietf:params:scim:schemas:core:2.0:Group"],
            |         "displayName": "Group B",
            |         "meta": {
            |           "resourceType": "Group",
            |           "created": "2011-08-01T18:29:50.873Z",
            |           "lastModified": "2011-08-01T18:29:50.873Z",
            |           "location": "https://example.com/v2/Groups/6c5bb468-14b2-4183-baf2-06d523e03bd3",
            |           "version": "W\/\"wGB85s2QJMjiNnuI\""
            |         },
            |         "members": [
            |           {
            |             "value": "c3a26dd3-27a0-4dec-a2ac-ce211e105f97",
            |             "$ref":"https://example.com/v2/Groups/c3a26dd3-27a0-4dec-a2ac-ce211e105f97",
            |             "type": "Group"
            |           },
            |           {
            |             "display": "Babs Jensen",
            |             "$ref": "https://example.com/v2/Users/2819c223...413861904646",
            |             "value": "2819c223-7f76-453a-919d-413861904646"
            |           }
            |         ]
            |       }
            |""".stripMargin
        )

        patch.applyTo(Schema.Group)(before) should be(Right(after))
      }

      it("should add a first member to a group (element to Null->array)") {
        val patch = patchOp(
          """
            |   { "schemas":
            |      ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
            |     "Operations":[
            |       {
            |        "op":"add",
            |        "path":"members",
            |        "value":[
            |         {
            |           "display": "Babs Jensen",
            |           "$ref":
            |   "https://example.com/v2/Users/2819c223...413861904646",
            |           "value": "2819c223-7f76-453a-919d-413861904646"
            |         }
            |        ]
            |       }
            |     ]
            |   }
            |""".stripMargin
        )

        val before = Jsons.groupEmpty

        val after = parseJson(
          """
            |       {
            |         "id": "6c5bb468-14b2-4183-baf2-06d523e03bd3",
            |         "schemas": ["urn:ietf:params:scim:schemas:core:2.0:Group"],
            |         "displayName": "Group C",
            |         "meta": {
            |           "resourceType": "Group",
            |           "created": "2011-08-01T18:29:50.873Z",
            |           "lastModified": "2011-08-01T18:29:50.873Z",
            |           "location": "https://example.com/v2/Groups/6c5bb468-14b2-4183-baf2-06d523e03bd3",
            |           "version": "W\/\"wGB85s2QJMjiNnuI\""
            |         },
            |         "members": [
            |           {
            |             "display": "Babs Jensen",
            |             "$ref": "https://example.com/v2/Users/2819c223...413861904646",
            |             "value": "2819c223-7f76-453a-919d-413861904646"
            |           }
            |         ]
            |       }
            |""".stripMargin
        )

        patch.applyTo(Schema.Group)(before) should be(Right(after))
      }

      it("should add new attribute") {
        val patch = patchOp(
          """
            |   {
            |     "schemas":
            |       ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
            |     "Operations":[{
            |       "op":"add",
            |       "value":{
            |         "emails":[
            |           {
            |             "value":"babs@jensen.org",
            |             "type":"home"
            |           }
            |         ],
            |         "nickname":"Babs"
            |       }
            |     }]
            |   }
            |""".stripMargin
        )

        val before = Jsons.userMinimal

        val after = parseJson(
          """
            |{
            |  "schemas": ["urn:ietf:params:scim:schemas:core:2.0:User"],
            |  "id": "2819c223-7f76-453a-919d-413861904646",
            |  "userName": "bjensen@example.com",
            |  "meta": {
            |    "resourceType": "User",
            |    "created": "2010-01-23T04:56:22Z",
            |    "lastModified": "2011-05-13T04:42:34Z",
            |    "version": "W\/\"3694e05e9dff590\"",
            |    "location":
            |     "https://example.com/v2/Users/2819c223-7f76-453a-919d-413861904646"
            |  },
            |  "emails": [
            |    {
            |      "value": "babs@jensen.org",
            |      "type": "home"
            |    }
            |  ],
            |  "nickname":"Babs"
            |}
            |""".stripMargin
        )

        patch.applyTo(Schema.User)(before) should be(Right(after))
      }

      it("should add new sub attribute") {
        val patch = patchOp(
          """
            |   {
            |     "schemas":
            |       ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
            |     "Operations":[{
            |      "op": "Add",
            |      "path": "name.givenName",
            |      "value": "Barbara"
            |     }]
            |   }
            |""".stripMargin
        )

        val before = Jsons.userMinimal

        val after = parseJson(
          """
            |{
            |  "schemas": ["urn:ietf:params:scim:schemas:core:2.0:User"],
            |  "id": "2819c223-7f76-453a-919d-413861904646",
            |  "userName": "bjensen@example.com",
            |  "meta": {
            |    "resourceType": "User",
            |    "created": "2010-01-23T04:56:22Z",
            |    "lastModified": "2011-05-13T04:42:34Z",
            |    "version": "W\/\"3694e05e9dff590\"",
            |    "location":
            |     "https://example.com/v2/Users/2819c223-7f76-453a-919d-413861904646"
            |  },
            |  "name": {
            |    "givenName": "Barbara"
            |  }
            |}
            |""".stripMargin
        )

        patch.applyTo(Schema.User)(before) should be(Right(after))
      }

      it("should replace members of a group (array content)") {
        val patch = patchOp(
          """
            |   {
            |     "schemas":
            |       ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
            |     "Operations": [{
            |       "op":"replace",
            |       "path":"members",
            |       "value":[
            |         {
            |           "display": "Babs Jensen",
            |           "$ref": "https://example.com/v2/Users/2819c223...413861904646",
            |           "value": "2819c223...413861904646"
            |         },
            |         {
            |           "display": "James Smith",
            |           "$ref": "https://example.com/v2/Users/08e1d05d...473d93df9210",
            |           "value": "08e1d05d...473d93df9210"
            |         }
            |       ]
            |     }]
            |   }
            """.stripMargin
        )

        val before = Jsons.group

        val r = patch.applyTo(Schema.Group)(before)
        val after = Group(r.getOrElse(fail(r.toString))).rootOrDefault
        after.id.value should be("6c5bb468-14b2-4183-baf2-06d523e03bd3")
        val ms = after.members.value
        ms should have size 2
        val m0 = ms.head
        val m1 = ms.drop(1).head
        m0.value should be("2819c223...413861904646")
        m0.display.value should be("Babs Jensen")
        m1.value should be("08e1d05d...473d93df9210")
        m1.display.value should be("James Smith")
      }

      it("should replace attribute with filter") {
        val patch = patchOp(
          """
            |   {
            |     "schemas":
            |       ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
            |     "Operations": [{
            |       "op":"replace",
            |       "path":"addresses[type eq \"work\"]",
            |       "value":
            |       {
            |         "type": "work",
            |         "streetAddress": "Junkerngasse 39",
            |         "locality": "Bern",
            |         "postalCode": "3011",
            |         "country": "CH",
            |         "formatted": "Junkerngasse 39\n3011 Bern\nSwitzerland",
            |         "primary": true
            |       }
            |     }]
            |   }
            """.stripMargin
        )

        val before = Jsons.userFull

        val r = patch.applyTo(Schema.Group)(before)
        val after = User(r.getOrElse(fail(r.toString))).rootOrDefault
        after.addresses.value should have size 2
        val home = after.addresses.value.find(_.`type`.value == "home").value
        home.country.value should be("USA")
        home.locality.value should be("Hollywood")
        val work = after.addresses.value.find(_.`type`.value == "work").value
        work.country.value should be("CH")
        work.locality.value should be("Bern")
        work.region should be(None)
        work.streetAddress.value should be("Junkerngasse 39")
      }

      it("should replace sub attribute") {
        val patch = patchOp(
          """
            |   {
            |     "schemas":
            |       ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
            |     "Operations": [{
            |       "op":"replace",
            |       "path":"name.familyName",
            |       "value":"McDonald"
            |     }]
            |   }
            |
            """.stripMargin
        )

        val before = Jsons.userFull

        val r = patch.applyTo(Schema.Group)(before)
        val after = User(r.getOrElse(fail(r.toString))).rootOrDefault
        after.name.value.familyName.value should be("McDonald")
        after.name.value.givenName.value should be("Barbara")
      }

      it("should replace sub attribute with filter") {
        val patch = patchOp(
          """
            |   {
            |     "schemas":
            |       ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
            |     "Operations": [{
            |       "op":"replace",
            |       "path":"addresses[type eq \"work\"].streetAddress",
            |       "value":"1111 Broadway Ave"
            |     }]
            |   }
            |
            """.stripMargin
        )

        val before = Jsons.userFull

        val r = patch.applyTo(Schema.Group)(before)
        val after = User(r.getOrElse(fail(r.toString))).rootOrDefault
        after.addresses.value should have size 2
        val home = after.addresses.value.find(_.`type`.value == "home").value
        home.streetAddress.value should be("456 Hollywood Blvd")
        val work = after.addresses.value.find(_.`type`.value == "work").value
        work.streetAddress.value should be("1111 Broadway Ave")
      }

      it("should remove attribute") {
        val patch = patchOp(
          """
            |   {
            |     "schemas":
            |       ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
            |     "Operations":[{
            |       "op":"remove",
            |       "path":"members"
            |     }]
            |   }
            |
            """.stripMargin
        )

        val before = Jsons.group

        val r = patch.applyTo(Schema.Group)(before)
        val after = Group(r.getOrElse(fail(r.toString))).rootOrDefault
        after.members should be(None)
      }

      it("should remove sub attribute") {
        val patch = patchOp(
          """
            |   {
            |     "schemas":
            |       ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
            |     "Operations":[{
            |       "op":"remove",
            |       "path":"addresses.region"
            |     }]
            |   }
            |
            """.stripMargin
        )

        val before = Jsons.userFull

        val r = patch.applyTo(Schema.Group)(before)
        val after = User(r.getOrElse(fail(r.toString))).rootOrDefault
        after.addresses.value should have size 2
        val home = after.addresses.value.find(_.`type`.value == "home").value
        home.streetAddress.value should be("456 Hollywood Blvd")
        home.region should be(None)
        val work = after.addresses.value.find(_.`type`.value == "work").value
        work.streetAddress.value should be("100 Universal City Plaza")
        work.region should be(None)
      }

      it("should remove attribute with filter") {
        val patch = patchOp(
          """
            |   {
            |     "schemas":
            |       ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
            |     "Operations":[{
            |       "op":"remove",
            |       "path":"members[value eq \"2819c223-7f76-...413861904646\"]"
            |     }]
            |   }
            |
            """.stripMargin
        )

        val before = Jsons.group2

        val r = patch.applyTo(Schema.Group)(before)
        val after = Group(r.getOrElse(fail(r.toString))).rootOrDefault
        after.members.value should have size 1
        after.members.value.head.value should be("c3a26dd3-27a0-4dec-a2ac-ce211e105f97")
      }

      it("should remove sub attribute with filter") {
        val patch = patchOp(
          """
            |   {
            |     "schemas":
            |       ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
            |     "Operations":[{
            |       "op":"remove",
            |       "path":"addresses[type eq \"home\"].region"
            |     }]
            |   }
            """.stripMargin
        )

        val before = Jsons.userFull

        val r = patch.applyTo(Schema.Group)(before)
        val after = User(r.getOrElse(fail(r.toString))).rootOrDefault
        after.addresses.value should have size 2
        val home = after.addresses.value.find(_.`type`.value == "home").value
        home.streetAddress.value should be("456 Hollywood Blvd")
        home.region should be(None)
        val work = after.addresses.value.find(_.`type`.value == "work").value
        work.streetAddress.value should be("100 Universal City Plaza")
        work.region.value should be("CA")
      }
    }
  }
}
