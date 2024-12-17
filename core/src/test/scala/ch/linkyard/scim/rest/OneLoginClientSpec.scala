package ch.linkyard.scim.rest

import cats.*
import io.circe.parser.parse
import org.scalatest.BeforeAndAfterEach
import org.scalatest.OptionValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import ch.linkyard.scim.model.Codecs.given
import ch.linkyard.scim.model.Group
import ch.linkyard.scim.model.ListResponse
import ch.linkyard.scim.model.User
import ch.linkyard.scim.rest.MockStore.MockOptimizedGroupStore
import ch.linkyard.scim.rest.MockStore.MockUserStore
import ch.linkyard.scim.rest.Resource.Path
import ch.linkyard.scim.rest.TestHelpers.*

import java.net.URI

/** Simulates calls as done by OneLogin as of 2020-03-21. */
class OneLoginClientSpec extends AnyFunSpec with Matchers with OptionValues with BeforeAndAfterEach {
  private implicit object Users extends MockUserStore
  private implicit object Groups extends MockOptimizedGroupStore

  private val urlRoot = "https://host.local/scim/v2"
  private val base = URI.create(urlRoot)
  private val api = RestApi[Id](UrlConfig(urlRoot))

  private val root: Path = Seq.empty

  override def beforeEach(): Unit = {
    Users.content = Seq.empty
    Groups.content = Seq.empty
  }

  describe("client OneLogin") {
    val user1 = User(User.Root(
      userName = "hans.mueller",
      id = Some("a-id-1"),
      meta = Some(User.userMeta("a-id-1").resolveLocation(base)),
    ))
    val user1b = User(User.Root(
      userName = "hans.mueller",
      id = user1.id,
      displayName = Some("Mülli"),
      meta = Some(User.userMeta("a-id-1").resolveLocation(base)),
    ))
    val user2 = User(User.Root(
      userName = "peter.meier",
      id = Some("a-id-2"),
      meta = Some(User.userMeta("a-id-1").resolveLocation(base)),
    ))
    val group1 = Group(Group.Root(
      displayName = "Group A",
      id = Some("g-a"),
      meta = Some(Group.groupMeta("g-a").resolveLocation(base)),
    ))
    val group2 = Group(Group.Root(
      displayName = "Group B",
      id = Some("g-b"),
      meta = Some(Group.groupMeta("g-b").resolveLocation(base)),
    ))

    it("should activate") {
      Users.content = Seq(user1, user2)
      val r = api.user.get(root, Map("filter" -> "userName eq \"never-existing-user\""))
      r.status should be(200)
      val lr = r.body.value.as[ListResponse].value
      lr.totalResults should be(0)
      lr.Resources should be(None)
    }

    it("should load existing groups") {
      Groups.content = Seq(group1, group2)
      val r = api.group.get(root, Map("count" -> "100", "startIndex" -> "1"))
      r.status should be(200)
      val lr = r.body.value.as[ListResponse].value
      lr.totalResults should be(2)
      val res = lr.Resources.value
      res should have size 2
      val g1 = res.head.as[Group].value
      g1.rootOrDefault.displayName should be("Group A")
      g1.id.value should be("g-a")
      val g2 = res(1).as[Group].value
      g2.rootOrDefault.displayName should be("Group B")
      g2.id.value should be("g-b")
    }

    it("should create a new user") {
      Users.content = Seq.empty

      // it checks first if the user already exists
      val r1 = api.user.get(root, Map("filter" -> "userName eq \"hans.mueller\""))
      r1.status should be(200)
      r1.body.value.as[ListResponse].value.totalResults should be(0)

      // Create the user
      val r2 = api.user.post(
        root,
        Map.empty,
        body = parse(
          """{"userName":"peter.meier@example.com","name":{"givenName":"Peter","familyName":"Meier"},
            |"schemas":["urn:ietf:params:scim:schemas:core:2.0:User"],"emails":[{"primary":true,"type":"work","value":"pm@example.com"}],
            |"active":true}""".stripMargin
        ).value,
      )
      if r2.status != 204 then {
        r2.status should be(200)
        val user = r2.body.value.as[User].value
        user.id.isDefined should be(true)
        user.rootOrDefault.userName should be("peter.meier@example.com")
        Users.content should have size 1
        Users.content.head.rootOrDefault.userName should be("peter.meier@example.com")
        ()
      }
    }

    it("should create a new user (with a group assigned)") {
      Users.content = Seq.empty
      Groups.content = Seq(group1)

      // Check if user exists
      val r1 = api.user.get(root, Map("filter" -> "userName eq \"hans.mueller\""))
      r1.status should be(200)
      r1.body.value.as[ListResponse].value.totalResults should be(0)

      // Create the user
      val r2 = api.user.post(
        root,
        Map.empty,
        body = parse(
          """{"userName":"peter.meier@example.com","name":{"givenName":"Peter","familyName":"Meier"},
            |"schemas":["urn:ietf:params:scim:schemas:core:2.0:User"],"emails":[{"primary":true,"type":"work","value":"pm@example.com"}],
            |"active":true}""".stripMargin
        ).value,
      )
      r2.status should be(200)
      val user = r2.body.value.as[User].value
      user.id.isDefined should be(true)
      user.rootOrDefault.userName should be("peter.meier@example.com")
      Users.content should have size 1
      Users.content.head.rootOrDefault.userName should be("peter.meier@example.com")
      val uid = user.id.value

      // Look for groups
      val r3 = api.group.get(root, Map("count" -> "100", "startIndex" -> "1"))
      r3.status should be(200)
      val lr = r3.body.value.as[ListResponse].value
      lr.totalResults should be(1)
      lr.Resources.value.head.as[Group].value should be(group1)

      // Create missing group
      val r4 = api.group.post(
        root,
        Map.empty,
        body = parse("""{"displayName":"Group C","schemas":["urn:ietf:params:scim:schemas:core:2.0:Group"]}""").value,
      )
      r4.status should be(200)
      val gc = r4.body.value.as[Group].value
      gc.id.isDefined should be(true)
      val gcId = gc.id.value
      gc.rootOrDefault.displayName should be("Group C")

      // Add user to group 1
      val r5 = api.group.patch(
        Seq("g-a"),
        Map.empty,
        body = parse(
          s"""{"schemas":["urn:ietf:params:scim:api:messages:2.0:PatchOp"],"Operations":[{"value":[{"value":"$uid"}],"op":"add",
             |"path":"members"}]}""".stripMargin
        )
          .value,
      )
      if r5.status != 204 then {
        r5.status should be(200)
        val g1 = r5.body.value.as[Group].value
        g1.id should be(group1.id)
        g1.rootOrDefault.members.value should have size 1
        g1.rootOrDefault.members.value.head.value should be(uid)
        ()
      }

      // Add user to group 2
      val r6 = api.group.patch(
        Seq(gcId),
        Map.empty,
        body = parse(
          s"""{"schemas":["urn:ietf:params:scim:api:messages:2.0:PatchOp"],"Operations":[{"value":[{"value":"$uid"}],"op":"add",
             |"path":"members"}]}""".stripMargin
        )
          .value,
      )
      if r6.status != 204 then {
        r6.status should be(200)
        val g2 = r6.body.value.as[Group].value
        g2.id should be(gc.id)
        g2.rootOrDefault.members.value should have size 1
        g2.rootOrDefault.members.value.head.value should be(uid)
        ()
      }

      Groups.content should have size 2
      val tg1 = Groups.content.head
      tg1.rootOrDefault.displayName should be("Group A")
      tg1.rootOrDefault.members.value should have size 1
      tg1.rootOrDefault.members.value.head.value should be(uid)
      val rg2 = Groups.content(1)
      rg2.rootOrDefault.displayName should be("Group C")
      rg2.rootOrDefault.members.value should have size 1
      rg2.rootOrDefault.members.value.head.value should be(uid)
    }

    it("should update user") {
      Users.content = Seq(user1, user2)
      val r = api.user.put(Seq("a-id-1"), Map.empty, body = user1b.asJson(base))
      r.status should be(200)
      r.body.value.as[User].value should be(user1b)
      // Users.content should have size 2
      // Users.content.find(_.id == user1.id).value.rootOrDefault.displayName.value should be("Mülli")
    }

    it("should add a new group to existing user") {
      Users.content = Seq(user1, user2)
      Groups.content = Seq(group1)

      // Update user
      val r1 = api.user.put(Seq("a-id-1"), Map.empty, body = user1.asJson(base))
      r1.status should be(200)
      r1.body.value.as[User].value should be(user1)

      // Look for groups
      val r2 = api.group.get(root, Map("count" -> "100", "startIndex" -> "1"))
      r2.status should be(200)
      val lr = r2.body.value.as[ListResponse].value
      lr.totalResults should be(1)
      lr.Resources.value.head.as[Group].value should be(group1)

      // Add user to group
      val r3 = api.group.patch(
        Seq("g-a"),
        Map.empty,
        body = parse(
          """{"schemas":["urn:ietf:params:scim:api:messages:2.0:PatchOp"],"Operations":[{"value":[{"value":"a-id-1"}],"op":"add",
            |"path":"members"}]}""".stripMargin
        )
          .value,
      )
      if r3.status != 204 then {
        r3.status should be(200)
        val g = r3.body.value.as[Group].value
        g.id should be(group1.id)
        g.rootOrDefault.members.value should have size 1
        g.rootOrDefault.members.value.head.value should be("a-id-1")
        ()
      }

      Groups.content should have size 1
      val g = Groups.content.head
      g.id should be(group1.id)
      g.rootOrDefault.members.value should have size 1
      g.rootOrDefault.members.value.head.value should be("a-id-1")
    }

    it("should add a new group to existing user when the group did not exist before") {
      Users.content = Seq(user1, user2)
      Groups.content = Seq(group1)

      // Update user
      val r1 = api.user.put(Seq("a-id-1"), Map.empty, body = user1.asJson(base))
      r1.status should be(200)
      r1.body.value.as[User].value should be(user1)

      // Look for groups
      val r2 = api.group.get(root, Map("count" -> "100", "startIndex" -> "1"))
      r2.status should be(200)
      val lr = r2.body.value.as[ListResponse].value
      lr.totalResults should be(1)
      lr.Resources.value.head.as[Group].value should be(group1)

      // Create group
      val r3 = api.group.post(
        root,
        Map.empty,
        body = parse("""{"displayName":"Group C","schemas":["urn:ietf:params:scim:schemas:core:2.0:Group"]}""").value,
      )
      r3.status should be(200)
      val gc = r3.body.value.as[Group].value
      gc.id.isDefined should be(true)
      val gcId = gc.id.value
      gc.rootOrDefault.displayName should be("Group C")

      // Add user to group
      val r4 = api.group.patch(
        Seq(gcId),
        Map.empty,
        body = parse(
          """{"schemas":["urn:ietf:params:scim:api:messages:2.0:PatchOp"],"Operations":[{"value":[{"value":"a-id-1"}],"op":"add",
            |"path":"members"}]}""".stripMargin
        )
          .value,
      )
      if r4.status != 204 then {
        r4.status should be(200)
        val g = r4.body.value.as[Group].value
        g.id should be(gc.id)
        g.rootOrDefault.members.value should have size 1
        g.rootOrDefault.members.value.head.value should be("a-id-1")
        ()
      }

      Groups.content should have size 2
      val g = Groups.content(1)
      g.id should be(gc.id)
      g.rootOrDefault.members.value should have size 1
      g.rootOrDefault.members.value.head.value should be("a-id-1")
    }

    it("should deactivate user") {
      Users.content = Seq(user1)
      Groups.content =
        Seq(Group(group1.rootOrDefault.copy(members = Some(Seq(Group.Member(user1.id.get), Group.Member(user2.id.get))))))

      // Remove user from group
      val r1 = api.group.patch(
        Seq("g-a"),
        Map.empty,
        body = parse(
          s"""{"schemas":["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
             |"Operations":[{"value":[{"value":"${user1.id.get}"}],"op":"remove","path":"members"}]}""".stripMargin
        ).value,
      )
      if r1.status != 204 then {
        r1.status should be(200)
        Group(r1.body.value).rootOrDefault.members should be(None)
        ()
      }

      // Set user to inactive
      val r2 = api.user.put(
        Seq(user1.id.get),
        Map.empty,
        body = User(user1.rootOrDefault.copy(active = Some(false))).asJson(base),
      )
      r2.status should be(200)
      User(r2.body.value).rootOrDefault.active should be(Some(false))

      Users.content should have size 1
      Users.content.head should be(User(r2.body.value))
      Groups.content should have size 1
      Groups.content.head.rootOrDefault.members should be(Some(Seq(Group.Member(user2.id.get))))
    }

    it("should delete user") {
      Users.content = Seq(user1)
      Groups.content = Seq(Group(group1.rootOrDefault.copy(members = Some(Seq(Group.Member(user1.id.get))))))

      // Remove user from group
      val r1 = api.group.patch(
        Seq("g-a"),
        Map.empty,
        body = parse(
          s"""{"schemas":["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
             |"Operations":[{"value":[{"value":"${user1.id.get}"}],"op":"remove","path":"members"}]}""".stripMargin
        ).value,
      )
      if r1.status != 204 then {
        r1.status should be(200)
        Group(r1.body.value).rootOrDefault.members should be(None)
        ()
      }

      // Delete user (guessed)
      val r2 = api.user.delete(Seq(user1.id.get), Map.empty)
      r2.status should be(204)

      Users.content should have size 0
      Groups.content should have size 1
      Groups.content.head.rootOrDefault.members.getOrElse(Nil) should have size 0
    }

    it("should sync users (with correct paging)") {
      def mkUser(i: Int) = User(User.Root(userName = s"user-$i", id = Some(s"user-$i")))

      val count = 500
      Users.content = (1 to count).map(mkUser).toList

      // Get users
      def getThem(pos: Int = 1, pageSize: Int = 100): Seq[User] = {
        val r = api.user.get(root, Map("count" -> pageSize.toString, "startIndex" -> pos.toString))
        r.status should be(200)
        val lr = r.body.value.as[ListResponse].value
        val start = lr.startIndex.getOrElse(1)
        start should be(pos)
        lr.itemsPerPage.value should be(pageSize)
        lr.totalResults should be(count)
        val es = lr.Resources.value.map(User.apply)
        if start + es.size < lr.totalResults then es ++ getThem(start + es.size, pageSize)
        else es
      }

      val r = getThem()
      r should have size count
      (1 to count).foreach { i =>
        r(i - 1).rootOrDefault.userName should be(s"user-$i")
      }
    }
  }

}
