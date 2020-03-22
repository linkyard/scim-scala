package scim.rest

import cats._
import io.circe.{Decoder, Json, ParsingFailure}
import org.scalatest.{BeforeAndAfterEach, OptionValues}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import scim.model.{Codecs, Group, ListResponse, Schema, User}
import scim.model.Codecs._
import scim.rest.Resource.Path
import scim.spi.{GroupStore, UserStore}
import io.circe.parser.parse

/** Simulates calls as done by OneLogin as of 2020-03-21. */
class OneLoginClientSpec extends AnyFunSpec with Matchers with OptionValues with BeforeAndAfterEach {
  private implicit object Users extends UserStore[Id] with MockStore[User] {
    override protected def schema = Schema.User
    override protected def duplicate(a: User, b: User) = a.root.userName == b.root.userName
    override protected implicit def decoder: Decoder[User] = Codecs.userDecoder
  }
  private implicit object Groups extends GroupStore[Id] with MockStore[Group] {
    override protected def schema = Schema.Group
    override protected def duplicate(a: Group, b: Group) = a.root.displayName == b.root.displayName
    override protected implicit def decoder: Decoder[Group] = Codecs.groupDecoder
  }

  private val urlRoot = "https://host.local/scim/v2"
  private val api = RestApi[Id](UrlConfig(urlRoot))

  private val root: Path = Seq.empty

  private implicit class ParserValues(r: Either[ParsingFailure, Json]) {
    def value: Json = r.getOrElse(fail(r.left.getOrElse(fail()).message))
  }
  private implicit class DecoderValues[A](r: Decoder.Result[A]) {
    def value: A = r.getOrElse(fail(r.left.getOrElse(fail()).message))
  }

  override def beforeEach(): Unit = {
    Users.content = Seq.empty
    Groups.content = Seq.empty
  }

  describe("client OneLogin") {
    val user1 = User(User.Root(userName = "hans.mueller", id = Some("a-id-1")))
    val user1b = User(User.Root(userName = "hans.mueller", id = user1.id, displayName = Some("Mülli")))
    val user2 = User(User.Root(userName = "peter.meier", id = Some("a-id-2")))
    val group1 = Group(Group.Root(displayName = "Group A", id = Some("g-a")))
    val group2 = Group(Group.Root(displayName = "Group B", id = Some("g-b")))

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
      res should have size (2)
      val g1 = res.head.as[Group].value
      g1.root.displayName should be("Group A")
      g1.id.value should be("g-a")
      val g2 = res(1).as[Group].value
      g2.root.displayName should be("Group B")
      g2.id.value should be("g-b")
    }

    it("should create a new user") {
      Users.content = Seq.empty

      // it checks first if the user already exists
      val r1 = api.user.get(root, Map("filter" -> "userName eq \"hans.mueller\""))
      r1.status should be(200)
      r1.body.value.as[ListResponse].value.totalResults should be(0)

      // Create the user
      val r2 = api.user.post(root, Map.empty,
        body = parse(
          """{"userName":"peter.meier@example.com","name":{"givenName":"Peter","familyName":"Meier"},
            |"schemas":["urn:ietf:params:scim:schemas:core:2.0:User"],"emails":[{"primary":true,"type":"work","value":"pm@example.com"}],
            |"active":true}""".stripMargin).value)
      r2.status should be(200)
      val user = r2.body.value.as[User].value
      user.id.isDefined should be(true)
      user.root.userName should be("peter.meier@example.com")
      Users.content should have size 1
      Users.content.head.root.userName should be("peter.meier@example.com")
    }

    it("should create a new user (with a group assigned)") {
      Users.content = Seq.empty
      Groups.content = Seq(group1)

      //Check if user exists
      val r1 = api.user.get(root, Map("filter" -> "userName eq \"hans.mueller\""))
      r1.status should be(200)
      r1.body.value.as[ListResponse].value.totalResults should be(0)

      // Create the user
      val r2 = api.user.post(root, Map.empty,
        body = parse(
          """{"userName":"peter.meier@example.com","name":{"givenName":"Peter","familyName":"Meier"},
            |"schemas":["urn:ietf:params:scim:schemas:core:2.0:User"],"emails":[{"primary":true,"type":"work","value":"pm@example.com"}],
            |"active":true}""".stripMargin).value)
      r2.status should be(200)
      val user = r2.body.value.as[User].value
      user.id.isDefined should be(true)
      user.root.userName should be("peter.meier@example.com")
      Users.content should have size 1
      Users.content.head.root.userName should be("peter.meier@example.com")
      val uid = user.id.value

      //Look for groups
      val r3 = api.group.get(root, Map("count" -> "100", "startIndex" -> "1"))
      r3.status should be(200)
      val lr = r3.body.value.as[ListResponse].value
      lr.totalResults should be(1)
      lr.Resources.value.head.as[Group].value should be(group1)

      //Create missing group
      val r4 = api.group.post(root, Map.empty,
        body = parse("""{"displayName":"Group C","schemas":["urn:ietf:params:scim:schemas:core:2.0:Group"]}""").value)
      r4.status should be(200)
      val gc = r4.body.value.as[Group].value
      gc.id.isDefined should be(true)
      val gcId = gc.id.value
      gc.root.displayName should be("Group C")

      //Add user to group 1
      val r5 = api.group.patch(Seq("g-a"), Map.empty,
        body = parse(
          s"""{"schemas":["urn:ietf:params:scim:api:messages:2.0:PatchOp"],"Operations":[{"value":[{"value":"$uid"}],"op":"add",
            |"path":"members"}]}""".stripMargin)
          .value)
      r5.status should be(200)
      val g1 = r5.body.value.as[Group].value
      g1.id should be(group1.id)
      g1.root.members.value should have size 1
      g1.root.members.value.head.value should be(uid)

      //Add user to group 2
      val r6 = api.group.patch(Seq(gcId), Map.empty,
        body = parse(
          s"""{"schemas":["urn:ietf:params:scim:api:messages:2.0:PatchOp"],"Operations":[{"value":[{"value":"$uid"}],"op":"add",
            |"path":"members"}]}""".stripMargin)
          .value)
      r6.status should be(200)
      val g2 = r6.body.value.as[Group].value
      g2.id should be(gc.id)
      g2.root.members.value should have size 1
      g2.root.members.value.head.value should be(uid)

      Groups.content should have size 2
      val tg1 = Groups.content.head
      tg1.root.displayName should be("Group A")
      tg1.root.members.value should have size 1
      tg1.root.members.value.head.value should be(uid)
      val rg2 = Groups.content(1)
      rg2.root.displayName should be("Group C")
      rg2.root.members.value should have size 1
      rg2.root.members.value.head.value should be(uid)
    }

    it("should update user") {
      Users.content = Seq(user1, user2)
      val r = api.user.put(Seq("a-id-1"), Map.empty, body = user1b.asJson)
      r.status should be(200)
      r.body.value.as[User].value should be(user1b)
      Users.content should have size (2)
      Users.content.find(_.id == user1.id).value.root.displayName.value should be("Mülli")
    }

    it("should add a new group to existing user") {
      Users.content = Seq(user1, user2)
      Groups.content = Seq(group1)

      // Update user
      val r1 = api.user.put(Seq("a-id-1"), Map.empty, body = user1.asJson)
      r1.status should be(200)
      r1.body.value.as[User].value should be(user1)

      //Look for groups
      val r2 = api.group.get(root, Map("count" -> "100", "startIndex" -> "1"))
      r2.status should be(200)
      val lr = r2.body.value.as[ListResponse].value
      lr.totalResults should be(1)
      lr.Resources.value.head.as[Group].value should be(group1)

      //Add user to group
      val r3 = api.group.patch(Seq("g-a"), Map.empty,
        body = parse(
          """{"schemas":["urn:ietf:params:scim:api:messages:2.0:PatchOp"],"Operations":[{"value":[{"value":"a-id-1"}],"op":"add",
            |"path":"members"}]}""".stripMargin)
          .value)
      r3.status should be(200)
      val g = r3.body.value.as[Group].value
      g.id should be(group1.id)
      g.root.members.value should have size 1
      g.root.members.value.head.value should be("a-id-1")

      Groups.content should have size 1
      Groups.content.head should be(g)
    }

    it("should add a new group to existing user when the group did not exist before") {
      Users.content = Seq(user1, user2)
      Groups.content = Seq(group1)

      // Update user
      val r1 = api.user.put(Seq("a-id-1"), Map.empty, body = user1.asJson)
      r1.status should be(200)
      r1.body.value.as[User].value should be(user1)

      //Look for groups
      val r2 = api.group.get(root, Map("count" -> "100", "startIndex" -> "1"))
      r2.status should be(200)
      val lr = r2.body.value.as[ListResponse].value
      lr.totalResults should be(1)
      lr.Resources.value.head.as[Group].value should be(group1)

      //Create group
      val r3 = api.group.post(root, Map.empty,
        body = parse("""{"displayName":"Group C","schemas":["urn:ietf:params:scim:schemas:core:2.0:Group"]}""").value)
      r3.status should be(200)
      val gc = r3.body.value.as[Group].value
      gc.id.isDefined should be(true)
      val gcId = gc.id.value
      gc.root.displayName should be("Group C")

      //Add user to group
      val r4 = api.group.patch(Seq(gcId), Map.empty,
        body = parse(
          """{"schemas":["urn:ietf:params:scim:api:messages:2.0:PatchOp"],"Operations":[{"value":[{"value":"a-id-1"}],"op":"add",
            |"path":"members"}]}""".stripMargin)
          .value)
      r4.status should be(200)
      val g = r4.body.value.as[Group].value
      g.id should be(gc.id)
      g.root.members.value should have size 1
      g.root.members.value.head.value should be("a-id-1")
      Groups.content should have size 2
      Groups.content(1) should be(g)
    }

    it("should deactivate user")(pending)

    it("should delete user")(pending)

    it("should sync users")(pending)
  }

}
