package scim.model

/** RFC 7644 3.12 */
case class Error(
  status: Int,
  detail: Option[String],
  scimType: Option[String] = None,
  schemas: Seq[Schema] = Seq(Schema.Error)) extends RootModel

object Error {

}
