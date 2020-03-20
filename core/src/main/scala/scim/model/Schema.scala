package scim.model

import java.net.URI
import scala.util.Try

case class Schema(uri: URI) {
  def asString: String = uri.toString
  override def toString: String = asString
}

object Schema {
  def apply(uri: String): Schema = Schema(URI.create(uri))
  def parse(uri: String): Either[String, Schema] = Try(URI.create(uri)).toEither
    .map(Schema.apply).left.map(_ => s"${uri} is not a valid schema")

  def default: Schema = None
  val None: Schema = Schema("urn:ietf:params:scim:schemas:core:2.0:NonExisting")

  val User: Schema = Schema("urn:ietf:params:scim:schemas:core:2.0:User")
  val Group: Schema = Schema("urn:ietf:params:scim:schemas:core:2.0:Group")
  val SearchRequest: Schema = Schema("urn:ietf:params:scim:api:messages:2.0:SearchRequest")
  val ListResponse: Schema = Schema("urn:ietf:params:scim:api:messages:2.0:ListResponse")
  val PatchOp: Schema = Schema("urn:ietf:params:scim:api:messages:2.0:PatchOp")
  val Error: Schema = Schema("urn:ietf:params:scim:api:messages:2.0:Error")
}
