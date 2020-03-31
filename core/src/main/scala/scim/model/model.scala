package scim.model

import io.circe.{DecodingFailure, Json}

trait Model

trait RootModel extends Model {
  def schemas: Seq[Schema]
}

trait ExtensibleModel[Root] extends Model {
  /** full/canonical representation */
  val json: Json

  def id: Option[String] = json.hcursor.downField("id").focus.flatMap(_.asString)
  def schema: Schema

  /** parsed representation */
  def root: Either[DecodingFailure, Root]
  def rootOrDefault: Root

  def asJson: Json = json
}
