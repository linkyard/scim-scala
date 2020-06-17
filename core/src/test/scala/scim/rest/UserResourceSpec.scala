package scim.rest

import java.net.URI
import cats.Id
import io.circe.parser.parse
import org.scalatest.OptionValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import scim.model.User
import scim.rest.TestHelpers._


class UserResourceSpec extends AnyFunSpec with Matchers with OptionValues {
  private def withResource[A](f: (UserResource[Id], MockStore[User]) => A): A = {
    implicit val store: MockStore.MockUserStore = new MockStore.MockUserStore {}
    val resource = new UserResource[Id](UrlConfig("https://host.local"))
    f(resource, store)
  }

  describe("User Resource") {
    val base = URI.create("https://host.local")

    val user1 = User(User.Root("user-1", id = Some("user-1"),
      meta = Some(User.userMeta("user-1").resolveLocation(base))))

    def tests(withIt: ((UserResource[Id], MockStore[User]) => Unit) => Unit): Unit = {
      it("should update email")(withIt { (rest: UserResource[Id], store: MockStore[User]) =>
        store.content = Seq(user1)
        val r = rest.patch(Seq("user-1"), Map.empty, body = parse(
          """{
            |  "schemas": [
            |    "urn:ietf:params:scim:api:messages:2.0:PatchOp"
            |  ],
            |  "Operations": [
            |    {
            |      "op": "replace",
            |      "value": {
            |        "emails": [
            |          {
            |            "type": "work",
            |            "value": "test-user@test.ch"
            |          }
            |        ]
            |      }
            |    }
            |  ]
            |}
            |""".stripMargin).value
        )
        r.status should be(200)

        store.content should have size 1
        val u = store.content.head
        u.rootOrDefault.emails.value
          .find(_.`type`.contains("work"))
          .value.value should be("test-user@test.ch")
      })

      it("should update displayName")(withIt { (rest: UserResource[Id], store: MockStore[User]) =>
        store.content = Seq(user1)
        val r = rest.patch(Seq("user-1"), Map.empty, body = parse(
          """{
            |  "schemas": [
            |    "urn:ietf:params:scim:api:messages:2.0:PatchOp"
            |  ],
            |  "Operations": [
            |    {
            |      "op": "replace",
            |      "value": {
            |        "displayName": "Tester 999"
            |      }
            |    }
            |  ]
            |}
            |""".stripMargin).value
        )
        r.status should be(200)

        store.content should have size 1
        val u = store.content.head
        u.rootOrDefault.displayName.value should be("Tester 999")
      })
    }

    describe("patching") {
      tests(withResource)
    }
  }
}
