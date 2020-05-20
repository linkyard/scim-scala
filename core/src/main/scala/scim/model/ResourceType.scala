package scim.model

import io.circe.Json
import scim.model.ResourceType.SchemaExtension

/** RFC 7643 chapter 6 */
case class ResourceType(
  id: String,
  externalId: Option[String] = None,
  name: String,
  description: String,
  endpoint: String,
  schema: Schema = Schema.ResourceType,
  schemaExtensions: Seq[SchemaExtension] = Nil
) extends JsonModel {
  def asJson: Json = Codecs.resourceTypeCode(this)
}

object ResourceType {
  case class SchemaExtension(schema: Schema, required: Boolean)

  object UserResourceType extends ResourceType(
    id = "User", endpoint = "/User",
    name = "User", description = "User Account",
    schema = Schema.User)
  object GroupResourceType extends ResourceType(
    id = "Group", endpoint = "/Group",
    name = "Group", description = "Group",
    schema = Schema.Group)
}
