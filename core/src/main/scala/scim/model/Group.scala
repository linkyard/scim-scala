package scim.model

import io.circe.Json
import io.circe.syntax._
import io.circe.generic.auto._
import scim.model.Group.Root

case class Group(json: Json) extends ExtensibleModel[Root] {
  def schema: Schema = Schema.Group
  lazy val root: Root = json.as[Root].toOption.getOrElse(Root.fallback)

  def ++(other: Json): Group = Group(json.deepMerge(other))
}

object Group {
  def apply(root: Root): Group = Group(root.asJson)

  case class Root(
    id: Option[String],
    displayName: String,
    members: Option[Seq[Member]] = None,
  )
  object Root {
    def fallback: Root = Root(id=None, displayName = "")
  }
  case class Member(
    value: String,
    `$ref`: Option[String] = None,
    display: Option[String] = None,
  ) {
    def id: String = value
  }
}

