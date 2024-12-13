package scim.rest

import cats.Applicative
import io.circe.Json
import scim.model.Filter
import scim.model.SchemaDefinition
import scim.rest.Helpers.Id
import scim.rest.Resource.Path
import scim.rest.Resource.QueryParams
import scim.spi.Paging
import scim.spi.SearchResult
import scim.spi.Sorting
import scim.spi.SpiError.DoesNotExist

import java.net.URI

class SchemasResource[F[_]](urlConfig: UrlConfig, schemas: Iterable[SchemaDefinition])(implicit
  applicative: Applicative[F]
) extends Resource[F] {
  private def pure[A]: A => F[A] = applicative.pure

  def get(subPath: Path, queryParams: QueryParams): F[Response] = {
    Helpers.Get.retrieve(subPath, urlConfig.base)(doGet)
      .orElse(Helpers.Get.search(subPath, queryParams, urlConfig.base)(doSearch))
      .getOrElse(pure(Response.notImplemented))
  }

  private def doGet(id: Id): F[Either[DoesNotExist, SchemaDefinition]] = pure {
    schemas.find(_.schema.asString.equalsIgnoreCase(id))
      .toRight(DoesNotExist(id))
  }

  private def doSearch(filter: Filter, pageMode: Paging, sorting: Option[Sorting]): F[SearchResult[SchemaDefinition]] =
    pure {
      val filtered = schemas.filter(rt => filter.evaluate(rt.asJson(URI.create("urn:none")))).toSeq
      pageMode.applyTo(sorting.map(_.applyTo(filtered)).getOrElse(filtered))
    }

  def post(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = pure(Response.notImplemented)
  def put(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = pure(Response.notImplemented)
  def patch(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = pure(Response.notImplemented)
  def delete(subPath: Path, queryParams: QueryParams): F[Response] = pure(Response.notImplemented)
}
