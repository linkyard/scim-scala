package scim.model

import scala.io.Source
import io.circe.Json
import io.circe.parser._

case class SchemaDefinition(
  schema: Schema,
  definition: Json
)

object SchemaDefinition {
  def user: SchemaDefinition = SchemaDefinition(Schema.User, jsonFromClasspath("userSchema.json"))
  def group: SchemaDefinition = SchemaDefinition(Schema.Group, jsonFromClasspath("groupSchema.json"))
  def defaultSchemas = Seq(user, group)

  def jsonFromClasspath(name: String): Json = {
    val string = Source.fromInputStream(getClass.getResourceAsStream(name)).mkString
    parse(string).fold(throw _, json => json)
  }
}
