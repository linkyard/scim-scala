package scim.model

import java.net.{URI, URLEncoder}
import java.time.Instant
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

  override def asJson(base: URI): Json = {
    root.toOption.map(root => root.metaOrDefault.resolveLocation(base))
      .fold(json)(meta => json.deepMerge(Json.obj("meta" -> meta.asJson)))
      .deepDropNullValues
  }
  override def meta: Meta = rootOrDefault.metaOrDefault

  def ++(other: Json): Group = Group(json.deepMerge(other))
}

object Group {
  def apply(root: Root): Group = Group(root.asJson.deepDropNullValues)

  def groupMeta(id: String, created: Option[Instant] = None, lastModified: Option[Instant] = None, version: Option[String] = None): Meta = {
    val name = URLEncoder.encode(id, "UTF-8")
    Meta("Group", locationRelative = Some(s"/Groups/$name"), created = created, lastModified = lastModified, version = version)
  }

  case class Root(
    id: Option[String],
    displayName: String,
    members: Option[Seq[Member]] = None,
    meta: Option[Meta] = None
  ) {
    def metaOrDefault: Meta = groupMeta(id.getOrElse(""))
  }
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

