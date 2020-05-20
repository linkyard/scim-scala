package scim.rest

import cats.Applicative
import io.circe.Json
import scim.model.Codecs._
import scim.model.{Filter, ResourceType}
import scim.rest.Helpers.Id
import scim.rest.Resource.{Path, QueryParams}
import scim.spi.SpiError.DoesNotExist
import scim.spi.{Paging, SearchResult, Sorting}

case class ResourceTypeResource[F[_]](urlConfig: UrlConfig, resourceTypes: Iterable[ResourceType])(implicit applicative: Applicative[F]) extends Resource[F] {
  private def pure[A]: A => F[A] = applicative.pure

  def get(subPath: Path, queryParams: QueryParams): F[Response] = {
    Helpers.Get.retrieve(subPath, urlConfig.user)(doGet)
      .orElse(Helpers.Get.search(subPath, queryParams)(doSearch))
      .getOrElse(pure(Response.notImplemented))
  }

  private def doGet(id: Id): F[Either[DoesNotExist, ResourceType]] = pure {
    resourceTypes.find(_.id.equalsIgnoreCase(id))
      .toRight(DoesNotExist(id))
  }

  private def doSearch(filter: Filter, pageMode: Paging, sorting: Option[Sorting]): F[SearchResult[ResourceType]] = pure {
    val filtered = resourceTypes.filter(rt => filter.evaluate(rt.asJson)).toSeq
    pageMode.applyTo(sorting.map(_.applyTo(filtered)).getOrElse(filtered))
  }

  def post(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = pure(Response.notImplemented)
  def put(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = pure(Response.notImplemented)
  def patch(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = pure(Response.notImplemented)
  def delete(subPath: Path, queryParams: QueryParams): F[Response] = pure(Response.notImplemented)
}
