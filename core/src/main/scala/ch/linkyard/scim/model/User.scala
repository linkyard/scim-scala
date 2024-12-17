package ch.linkyard.scim.model

import ch.linkyard.scim.model.Codecs.given
import ch.linkyard.scim.model.User.Root
import io.circe.Decoder.Result
import io.circe.Json
import io.circe.generic.auto.*
import io.circe.syntax.*

import java.net.URI
import java.net.URLEncoder
import java.time.Instant
import java.util.Locale

case class User(json: Json) extends ExtensibleModel[Root]:
  def schema: Schema = Schema.User
  lazy val root: Result[Root] = json.as[Root]
  def rootOrDefault: Root = root.toOption.getOrElse(Root.fallback)

  override def asJson(base: URI): Json =
    root.toOption.map(root => root.metaOrDefault.resolveLocation(base))
      .fold(json)(meta => json.deepMerge(Json.obj("meta" -> meta.asJson)))
      .deepDropNullValues

  override def meta: Meta = rootOrDefault.metaOrDefault

  def ++(other: Json): User = User(json.deepMerge(other))

object User:
  def apply(root: Root): User = User(root.asJson.deepDropNullValues)

  def userMeta(
    id: String,
    created: Option[Instant] = None,
    lastModified: Option[Instant] = None,
    version: Option[String] = None,
  ): Meta = {
    val name = URLEncoder.encode(id, "UTF-8")
    Meta(
      "User",
      locationRelative = Some(s"/Users/$name"),
      created = created,
      lastModified = lastModified,
      version = version,
    )
  }

  case class Root(
    userName: String,
    id: Option[String],
    meta: Option[Meta] = None,
    name: Option[Name] = None,
    displayName: Option[String] = None,
    nickName: Option[String] = None,
    profileUrl: Option[URI] = None,
    title: Option[String] = None,
    userType: Option[String] = None,
    preferredLanguage: Option[String] = None,
    locale: Option[Locale] = None,
    timezone: Option[String] = None,
    active: Option[Boolean] = None,
    password: Option[String] = None,
    emails: Option[Seq[ValueWithTypeAndDisplay]] = None,
    addresses: Option[Seq[Address]] = None,
    phoneNumbers: Option[Seq[ValueWithTypeAndDisplay]] = None,
    ims: Option[Seq[ValueWithTypeAndDisplay]] = None,
    groups: Option[Seq[GroupRef]] = None,
    entitlements: Option[Seq[String]] = None,
    roles: Option[Seq[String]] = None,
    x509Certificates: Option[Seq[SimpleValue]] = None,
    `urn:ietf:params:scim:schemas:extension:enterprise:2.0:User`: Option[EnterpriseUser] = None,
    schemas: Option[List[String]] = Some(List(Schema.User.asString))
  ):
    def enterprise: Option[EnterpriseUser] = `urn:ietf:params:scim:schemas:extension:enterprise:2.0:User`
    def metaOrDefault: Meta = userMeta(id.getOrElse(userName))
  object Root:
    private[User] val fallback = Root(id = None, userName = "")

  case class Name(
    formatted: Option[String] = None,
    familyName: Option[String] = None,
    givenName: Option[String] = None,
    middleName: Option[String] = None,
    honorificPrefix: Option[String] = None,
    honorificSuffix: Option[String] = None,
  )
  case class Address(
    `type`: Option[String],
    formatted: Option[String],
    streetAddress: Option[String],
    locality: Option[String],
    region: Option[String],
    postalCode: Option[String],
    country: Option[String],
  )
  case class EnterpriseUser(
    employeeNumber: Option[String],
    costCenter: Option[String],
    organization: Option[String],
    division: Option[String],
    department: Option[String],
    manager: Option[UserRef],
  )
  case class SimpleValue(value: String)
  case class ValueWithTypeAndDisplay(value: Option[String], display: Option[String], `type`: Option[String])
  case class UserRef(
    value: String,
    `$ref`: Option[String],
    displayName: Option[String],
  ):
    def ref: Option[String] = $ref
  case class GroupRef(
    value: String,
    `$ref`: Option[String],
    display: Option[String],
    `type`: Option[String],
  ):
    def ref: Option[String] = $ref
