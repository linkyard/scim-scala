package ch.linkyard.scim.rest

import cats.Applicative
import ch.linkyard.scim.rest.Resource.Path
import ch.linkyard.scim.rest.Resource.QueryParams
import io.circe.Json

class NotImplementedResource[F[_]: Applicative] extends Resource[F]:
  override def get(subPath: Path, queryParams: QueryParams): F[Response] = Applicative[F].pure(Response.notImplemented)
  override def post(subPath: Path, queryParams: QueryParams, body: Json): F[Response] =
    Applicative[F].pure(Response.notImplemented)
  override def put(subPath: Path, queryParams: QueryParams, body: Json): F[Response] =
    Applicative[F].pure(Response.notImplemented)
  override def patch(subPath: Path, queryParams: QueryParams, body: Json): F[Response] =
    Applicative[F].pure(Response.notImplemented)
  override def delete(subPath: Path, queryParams: QueryParams): F[Response] =
    Applicative[F].pure(Response.notImplemented)
