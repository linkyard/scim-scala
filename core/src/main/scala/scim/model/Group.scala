package scim.model

import com.typesafe.scalalogging.LazyLogging
import io.circe.Decoder.Result
import io.circe.Json
import io.circe.generic.auto._
import io.circe.syntax._
import scim.model.Group.Root
import Codecs._

case class Group(json: Json) extends ExtensibleModel[Root] with LazyLogging {
  def schema: Schema = Schema.Group
  lazy val root: Result[Root] = json.as[Root]
  def rootOrDefault: Root = root.toOption.getOrElse(Root.fallback)

  def ++(other: Json): Group = Group(json.deepMerge(other))
}

object Group {
  def apply(root: Root): Group = Group(root.asJson.deepDropNullValues)

  case class Root(
    id: Option[String],
    displayName: String,
    members: Option[Seq[Member]] = None,
  )
  object Root {
    def fallback: Root = Root(id = None, displayName = "")
  }
  case class Member(
    value: String,
    `$ref`: Option[String] = None,
    display: Option[String] = None,
  ) {
    def id: String = value
  }
}

