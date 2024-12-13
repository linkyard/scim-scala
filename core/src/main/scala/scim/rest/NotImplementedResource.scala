package scim.rest

import cats.Applicative
import io.circe.Json
import scim.rest.Resource.Path
import scim.rest.Resource.QueryParams

class NotImplementedResource[F[_]: Applicative] extends Resource[F] {
  override def get(subPath: Path, queryParams: QueryParams): F[Response] = Applicative[F].pure(Response.notImplemented)
  override def post(subPath: Path, queryParams: QueryParams, body: Json): F[Response] =
    Applicative[F].pure(Response.notImplemented)
  override def put(subPath: Path, queryParams: QueryParams, body: Json): F[Response] =
    Applicative[F].pure(Response.notImplemented)
  override def patch(subPath: Path, queryParams: QueryParams, body: Json): F[Response] =
    Applicative[F].pure(Response.notImplemented)
  override def delete(subPath: Path, queryParams: QueryParams): F[Response] =
    Applicative[F].pure(Response.notImplemented)
}
