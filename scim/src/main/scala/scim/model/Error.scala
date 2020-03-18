package scim.model

/** RFC 7644 3.12 */
case class Error(
  status: Int,
  detail: Option[String],
  scimType: Option[String] = None,
  schemas: Seq[String] = Seq("urn:ietf:params:scim:api:messages:2.0:Error")) extends Root
