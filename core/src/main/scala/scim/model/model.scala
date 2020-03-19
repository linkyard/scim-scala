package scim.model

import io.circe.Json

trait Model

trait RootModel extends Model {
  def schemas: Seq[Schema]
}

trait ExtensibleModel[Root] extends Model {
  /** full/canonical representation */
  val json: Json

  def id: String = json.hcursor.downField("id").focus.flatMap(_.asString).getOrElse("")
  def schema: Schema

  /** parsed representation */
  def root: Root

  def asJson: Json = json
}
