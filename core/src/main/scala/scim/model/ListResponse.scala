package scim.model

import io.circe.Json

/** RFC 7644 3.4.3 */
case class ListResponse(
  totalResults: Int,
  /** 1-based */
  startIndex: Option[Int],
  itemsPerPage: Option[Int],
  Resources: Option[Seq[Json]],
  schemas: Seq[Schema] = Seq(Schema.ListResponse),
) extends RootModel
