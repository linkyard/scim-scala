package scim.model

/** RFC 7644 3.4.3 */
case class SearchRequest(
  attributes: Option[Seq[String]],
  excludedAttributes: Option[Seq[String]],
  filter: Option[Filter],
  sortBy: Option[String],
  sortOrder: Option[SortOrder],
  startIndex: Option[Int],
  count: Option[Int],

  schemas: Seq[Schema] = Seq(Schema.SearchRequest)) extends Root
