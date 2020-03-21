package scim.rest

import cats.{Applicative, Monad}
import io.circe.Json
import scim.rest.Resource.{Path, QueryParams}

private class GroupResource[F[_]](implicit monad: Monad[F]) extends Resource[F] {
  // TODO
  override def get(subPath: Path, queryParams: QueryParams) = Applicative[F].pure(Response.notImplemented)
  // TODO
  override def post(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = Applicative[F].pure(Response.notImplemented)
  // TODO
  override def put(subPath: Path, queryParams: QueryParams, body: Json) = Applicative[F].pure(Response.notImplemented)
  // TODO
  override def patch(subPath: Path, queryParams: QueryParams, body: Json) = Applicative[F].pure(Response.notImplemented)
  // TODO
  override def delete(subPath: Path, queryParams: QueryParams) = Applicative[F].pure(Response.notImplemented)
}
