package scim

import scim.model.SortOrder

package object spi {

  /** Start is 0-based (first result = index 0) */
  case class Paging(start: Int = 0, maxResults: Option[Int] = None)

  case class Sorting(byField: String, order: SortOrder)

  case class SearchResult[A](results: Seq[A], totalCount: Int)

}
