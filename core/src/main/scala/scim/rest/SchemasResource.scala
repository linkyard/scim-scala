package scim.rest

import cats.Applicative
import io.circe.Json
import scim.model.{Meta, SchemaDefinition}
import scim.rest.Resource.{Path, QueryParams}

class SchemasResource[F[_]](urlConfig: UrlConfig, schemas: Iterable[SchemaDefinition])(implicit applicative: Applicative[F]) extends Resource[F] {
  private def pure[A]: A => F[A] = applicative.pure

  def get(subPath: Path, queryParams: QueryParams): F[Response] = {
    val schemaName = Some(subPath.mkString("/")).filter(_.nonEmpty)
    schemaName match {
      case Some(name) =>
        pure(schemas.find(_.schema.asString == name)
          .map(schema => Response.ok(schema, urlConfig.base))
          .getOrElse(Response.notFound(name)))
      case None =>
        val jsons = schemas.map(_.asJson(urlConfig.base)).toSeq
        pure(Response.okJson(Json.arr(jsons: _*)))
    }
  }

  def post(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = pure(Response.notImplemented)
  def put(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = pure(Response.notImplemented)
  def patch(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = pure(Response.notImplemented)
  def delete(subPath: Path, queryParams: QueryParams): F[Response] = pure(Response.notImplemented)
}
