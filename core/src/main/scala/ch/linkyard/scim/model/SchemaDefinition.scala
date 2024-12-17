package ch.linkyard.scim.model

import ch.linkyard.scim.model.Codecs.given
import io.circe.Json
import io.circe.parser.*
import io.circe.syntax.*

import java.net.URI
import java.net.URLEncoder
import scala.io.Source

case class SchemaDefinition(
  schema: Schema,
  definition: Json,
) extends RootModel with JsonModel:
  override def schemas: Seq[Schema] = List(Schema.SchemaItself)

  override def meta: Meta =
    Meta("Schema", locationRelative = Some(s"/Schemas/${URLEncoder.encode(schema.asString, "UTF-8")}"))

  override def asJson(base: URI): Json = definition
    .deepMerge(Json.obj("meta" -> meta.resolveLocation(base).asJson))
    .deepMerge(Json.obj("schemas" -> Json.arr(schemas.map(_.asString).map(Json.fromString)*)))
end SchemaDefinition

object SchemaDefinition:
  def user: SchemaDefinition = SchemaDefinition(Schema.User, jsonFromClasspath("userSchema.json"))
  def group: SchemaDefinition = SchemaDefinition(Schema.Group, jsonFromClasspath("groupSchema.json"))
  def defaultSchemas: Seq[SchemaDefinition] = Seq(user, group)

  def jsonFromClasspath(name: String): Json =
    val string = Source.fromInputStream(getClass.getResourceAsStream(name)).mkString
    parse(string).fold(throw _, json => json)
