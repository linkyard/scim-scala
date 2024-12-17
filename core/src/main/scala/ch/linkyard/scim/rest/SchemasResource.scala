package ch.linkyard.scim.rest

import cats.Applicative
import cats.implicits.*
import ch.linkyard.scim.model.Filter
import ch.linkyard.scim.model.SchemaDefinition
import ch.linkyard.scim.rest.Helpers.Id
import ch.linkyard.scim.rest.Resource.Path
import ch.linkyard.scim.rest.Resource.QueryParams
import io.circe.Json
import scim.spi.Paging
import scim.spi.SearchResult
import scim.spi.Sorting
import scim.spi.SpiError.DoesNotExist

import java.net.URI

class SchemasResource[F[_]: Applicative](urlConfig: UrlConfig, schemas: Iterable[SchemaDefinition]) extends Resource[F]:
  def get(subPath: Path, queryParams: QueryParams): F[Response] =
    Helpers.Get.retrieve(subPath, urlConfig.base)(doGet)
      .orElse(Helpers.Get.search(subPath, queryParams, urlConfig.base)(doSearch))
      .getOrElse(Response.notImplemented.pure)

  private def doGet(id: Id): F[Either[DoesNotExist, SchemaDefinition]] =
    schemas.find(_.schema.asString.equalsIgnoreCase(id))
      .toRight(DoesNotExist(id)).pure

  private def doSearch(filter: Filter, pageMode: Paging, sorting: Option[Sorting]): F[SearchResult[SchemaDefinition]] =
    val filtered = schemas.filter(rt => filter.evaluate(rt.asJson(URI.create("urn:none")))).toSeq
    pageMode.applyTo(sorting.map(_.applyTo(filtered)).getOrElse(filtered)).pure

  def post(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = Response.notImplemented.pure
  def put(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = Response.notImplemented.pure
  def patch(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = Response.notImplemented.pure
  def delete(subPath: Path, queryParams: QueryParams): F[Response] = Response.notImplemented.pure
