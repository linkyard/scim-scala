package ch.linkyard.scim

import ch.linkyard.scim.model.JsonModel
import ch.linkyard.scim.model.SortOrder
import io.circe.Json

import java.net.URI

package object spi:

  /** Start is 0-based (first result = index 0) */
  case class Paging(start: Int = 0, maxResults: Option[Int] = None):
    /** Applies this paging to the sequence. Attention: use database-based paging to avoid loading all possible results.
      */
    def applyTo[A](allResults: Seq[A]): SearchResult[A] = {
      val page = allResults.slice(start, start + maxResults.getOrElse(Int.MaxValue))
      SearchResult(page, allResults.size)
    }
  end Paging

  given Ordering[Json] with
    override def compare(x: Json, y: Json): Int = x.fold(
      jsonNull = -1,
      jsonBoolean = _.compareTo(y.asBoolean.getOrElse(false): Boolean),
      jsonNumber = _.toDouble.compareTo(y.asNumber.map(_.toDouble).getOrElse(Double.MinValue): Double),
      jsonString = _.compareTo(y.asString.getOrElse("")),
      jsonArray = _ => -1,
      jsonObject = _ => -1,
    )

  case class Sorting(byField: String, order: SortOrder):
    /** Sorts the sequence according to the order defined by this sorting. Attention: use database-based sorts to avoid
      * loading all results.
      */
    def applyTo[A <: JsonModel](results: Seq[A]): Seq[A] =
      val sorted = results.sortBy(_.asJson(URI.create("urn:none")).hcursor.downField(byField).focus)
      if order == SortOrder.Descending then sorted.reverse
      else sorted
  end Sorting

  case class SearchResult[A](results: Seq[A], totalCount: Int)
