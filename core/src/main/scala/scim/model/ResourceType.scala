package scim.model

import java.net.URI
import io.circe.Json
import io.circe.syntax._
import Codecs._
import scim.model.ResourceType.SchemaExtension

/** RFC 7643 chapter 6 */
case class ResourceType(
  id: String,
  name: String,
  description: String,
  endpoint: String,
  schema: Schema = Schema.ResourceType,
  schemaExtensions: Seq[SchemaExtension] = Nil,
  schemas: Seq[Schema] = List(Schema.ResourceType),
) extends RootModel with JsonModel {
  def asJson(base: URI): Json = Codecs.resourceTypeCode(this)
    .deepMerge(Json.obj("meta" -> meta.resolveLocation(base).asJson))
  override def meta: Meta = Meta("ResourceType", locationRelative = Some(s"/ResourceType/$id"))
}

object ResourceType {
  case class SchemaExtension(schema: Schema, required: Boolean)

  object UserResourceType extends ResourceType(
    id = "User", endpoint = "/Users",
    name = "User", description = "User Account",
    schema = Schema.User)
  object GroupResourceType extends ResourceType(
    id = "Group", endpoint = "/Groups",
    name = "Group", description = "Group",
    schema = Schema.Group)
}
