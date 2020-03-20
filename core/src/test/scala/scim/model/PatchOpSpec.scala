package scim.model

import io.circe.syntax._
import io.circe.{Json, parser}
import io.circe.generic.auto._
import Codecs._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers
import Arbitraries._

class PatchOpSpec extends AnyFunSpec with Checkers with Matchers {
  describe("PatchOp") {
    it("should serialize/parse to same Json")(check { patch: PatchOp =>
      val json = patch.asJson
      val r = json.as[PatchOp]
      val reparsed = r.getOrElse(fail(s"could not parse: ${r}"))
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
            |""".stripMargin)

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
            |""".stripMargin)

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
            |""".stripMargin)

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
            |""".stripMargin)

        patch.applyTo(Schema.User)(before) should be(Right(after))
      }
    }
  }

}
