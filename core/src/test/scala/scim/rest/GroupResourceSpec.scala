package scim.rest

import cats.Id
import io.circe.parser.parse
import org.scalatest.funspec.AnyFunSpec
import scim.model.{Group, Jsons}
import scim.model.Group.Member
import TestHelpers._
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers

class GroupResourceSpec extends AnyFunSpec with Matchers with OptionValues {
  private def withResource[A](optimizable: Boolean)(f: (GroupResource[Id], MockStore[Group]) => A): A = {
    implicit val store: MockStore.MockGroupStore = new MockStore.MockGroupStore {}
    val resource = new GroupResource[Id](UrlConfig("https://host.local"))
    f(resource, store)
  }
  private def withResourceOptimized[A](optimizable: Boolean)(f: (GroupResource[Id], MockStore[Group]) => A): A = {
    implicit val store: MockStore.MockOptimizedGroupStore = new MockStore.MockOptimizedGroupStore {}
    val resource = new GroupResource[Id](UrlConfig("https://host.local"))
    val result = f(resource, store)
    store.wasOptimized should be(optimizable)
    result
  }

  describe("Group Resource") {
    val group1 = Group(Group.Root(id = Some("g1"), displayName = "Group 1", members = Some(Seq(Member("u-1")))))
    val group1b = Group(Group.Root(id = Some("g1"), displayName = "Group 1", members = Some(
      Seq(Member("u-1"), Member("u-2"), Member("u-3")))))
    val group2 = Group(Jsons.group2)

    def tests(withIt: (Boolean => ((GroupResource[Id], MockStore[Group]) => Unit) => Unit)): Unit = {
      it("should update if patched with single adds on member")(withIt(true) { (rest, store) =>
        store.content = Seq(group1, group2)
        val r = rest.patch(Seq("g1"), Map.empty,
          body = parse(
            """
              |{
              |  "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
              |  "Operations": [{
              |      "op": "add",
              |      "value": [
              |        { "value": "u-2", "display": "User 2" }
              |      ],
              |      "path": "members"
              |    }]
              |}
              |""".stripMargin).value
        )
        if (r.status == 200) {
          r.status should be(200)
          Group(r.body.value).id.value should be("g1")
          Group(r.body.value).rootOrDefault should be(Group.Root(id = Some("g1"), displayName = "Group 1",
            members = Some(Seq(Member("u-1"), Member("u-2", display = Some("User 2"))))))
        } else {
          r.status should be(204)
          r.body should be(None)
        }

        store.content should have size 2
        val members = store.content.find(_.id.contains("g1")).value.rootOrDefault.members.value
        members should have size 2
        members.exists(_.value == "u-1") should be(true)
        members.exists(_.value == "u-2") should be(true)
      })

      it("should update if patched with multiple adds on member")(withIt(true) { (rest, store) =>
        store.content = Seq(group1, group2)
        val r = rest.patch(Seq("g1"), Map.empty,
          body = parse(
            """
              |{
              |  "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
              |  "Operations": [{
              |      "op": "add",
              |      "value": [
              |        { "value": "u-2", "display": "User 2" },
              |        { "value": "u-3", "display": "User 3" },
              |        { "value": "u-4", "display": "User 4" }
              |      ],
              |      "path": "members"
              |    }]
              |}
              |""".stripMargin).value
        )
        if (r.status == 200) {
          r.status should be(200)
          Group(r.body.value).id.value should be("g1")
          Group(r.body.value).rootOrDefault should be(Group.Root(id = Some("g1"), displayName = "Group 1", members = Some(Seq(Member("u-1"),
            Member("u-2", display = Some("User 2")), Member("u-3", display = Some("User 3")), Member("u-4", display = Some("User 4"))))))
        } else {
          r.status should be(204)
          r.body should be(None)
        }

        store.content should have size 2
        val members = store.content.find(_.id.contains("g1")).value.rootOrDefault.members.value
        members should have size 4
        members.exists(_.value == "u-1") should be(true)
        members.exists(_.value == "u-2") should be(true)
        members.exists(_.value == "u-3") should be(true)
        members.exists(_.value == "u-4") should be(true)
      })

      it("should update if patched with single remove on member")(withIt(true) { (rest, store) =>
        store.content = Seq(group1b, group2)
        val r = rest.patch(Seq("g1"), Map.empty,
          body = parse(
            """
              |{
              |  "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
              |  "Operations": [{
              |      "op": "remove",
              |      "path": "members[value eq \"u-1\"]"
              |    }]
              |}
              |""".stripMargin).value
        )
        if (r.status == 200) {
          r.status should be(200)
          Group(r.body.value).id.value should be("g1")
          Group(r.body.value).rootOrDefault should be(Group.Root(id = Some("g1"), displayName = "Group 1",
            members = Some(Seq(Member("u-2"), Member("u-3")))))
        } else {
          r.status should be(204)
          r.body should be(None)
        }

        store.content should have size 2
        val members = store.content.find(_.id.contains("g1")).value.rootOrDefault.members.value
        members should have size 2
        members.exists(_.value == "u-2") should be(true)
        members.exists(_.value == "u-3") should be(true)
      })

      it("should update if patched with single remove on member (alternative/weird way)")(withIt(true) { (rest, store) =>
        store.content = Seq(group1b, group2)
        val r = rest.patch(Seq("g1"), Map.empty,
          body = parse(
            """
              |{
              |  "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
              |  "Operations": [{
              |      "op": "remove",
              |      "path": "members",
              |      "value": [{"value": "u-1"}]
              |    }]
              |}
              |""".stripMargin).value
        )
        if (r.status == 200) {
          r.status should be(200)
          Group(r.body.value).id.value should be("g1")
          Group(r.body.value).rootOrDefault should be(Group.Root(id = Some("g1"), displayName = "Group 1",
            members = Some(Seq(Member("u-2"), Member("u-3")))))
        } else {
          r.status should be(204)
          r.body should be(None)
        }

        store.content should have size 2
        val members = store.content.find(_.id.contains("g1")).value.rootOrDefault.members.value
        members should have size 2
        members.exists(_.value == "u-2") should be(true)
        members.exists(_.value == "u-3") should be(true)
      })

      it("should use update if patched with multiple remove on member")(withIt(false) { (rest, store) =>
        store.content = Seq(group1b, group2)
        val r = rest.patch(Seq("g1"), Map.empty,
          body = parse(
            """
              |{
              |  "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
              |  "Operations": [{
              |      "op": "remove",
              |      "path": "members[value ne \"u-1\"]"
              |    }]
              |}
              |""".stripMargin).value
        )
        if (r.status == 200) {
          r.status should be(200)
          Group(r.body.value).id.value should be("g1")
          Group(r.body.value).rootOrDefault should be(Group.Root(id = Some("g1"), displayName = "Group 1",
            members = Some(Seq(Member("u-1")))))
        } else {
          r.status should be(204)
          r.body should be(None)
        }

        store.content should have size 2
        val members = store.content.find(_.id.contains("g1")).value.rootOrDefault.members.value
        members should have size 1
        members.head.value should be("u-1")
      })
    }

    describe("unoptimized add/remove Member") {
      tests(withResource)
    }

    describe("optimized add/remove Member") {
      tests(withResourceOptimized)
    }
  }

}
