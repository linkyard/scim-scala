package scim.model

import java.net.URI
import java.util.Locale
import io.circe.Json
import io.circe.generic.auto._
import io.circe.syntax._
import scim.model.User.Root
import Codecs._

case class User(json: Json) extends ExtensibleModel[Root] {
  def schema: Schema = Schema.User
  lazy val root: Root = json.as[Root].toOption.getOrElse(Root.fallback)

  def ++(other: Json): User = User(json.deepMerge(other))
}

object User {
  def apply(root: Root): User = User(root.asJson.deepDropNullValues)

  case class Root(
    userName: String,
    id: Option[String],
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
    `urn:ietf:params:scim:schemas:extension:enterprise:2.0:User`: Option[EnterpriseUser] = None
  ) {
    def enterprise: Option[EnterpriseUser] = `urn:ietf:params:scim:schemas:extension:enterprise:2.0:User`
  }
  object Root {
    private[User] val fallback = Root(id = None, userName = "")
  }

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
  case class ValueWithTypeAndDisplay(value: String, display: Option[String], `type`: Option[String])
  case class UserRef(
    value: String,
    `$ref`: Option[String],
    displayName: Option[String],
  ) {
    def ref: Option[String] = $ref
  }
  case class GroupRef(
    value: String,
    `$ref`: Option[String],
    display: Option[String],
    `type`: Option[String],
  ) {
    def ref: Option[String] = $ref
  }
}
