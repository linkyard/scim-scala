package scim.rest

import cats.Applicative
import io.circe.Json
import scim.model.SchemaDefinition
import scim.rest.Resource.{Path, QueryParams}

case class SchemasResource[F[_]](schemas: Iterable[SchemaDefinition])(implicit applicative: Applicative[F]) extends Resource[F] {
  private def pure[A]: A => F[A] = applicative.pure

  def get(subPath: Path, queryParams: QueryParams): F[Response] = {
    Some(subPath.mkString("/")).filter(_.nonEmpty) match {
      case Some(name) =>
        pure(schemas.find(_.schema.asString == name)
          .map(schema => Response.ok(schema.definition))
          .getOrElse(Response.notFound(name)))
      case None =>
        pure(Response.ok(Json.arr(schemas.map(_.definition).toSeq: _*)))
    }
  }

  def post(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = pure(Response.notImplemented)
  def put(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = pure(Response.notImplemented)
  def patch(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = pure(Response.notImplemented)
  def delete(subPath: Path, queryParams: QueryParams): F[Response] = pure(Response.notImplemented)
}
