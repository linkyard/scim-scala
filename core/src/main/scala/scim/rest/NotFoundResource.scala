package scim.rest

import cats.{Applicative, Functor}
import io.circe.Json

class NotFoundResource[F[_] : Applicative] extends Resource[F] {
  override def get(subPath: Path, queryParams: QueryParams): F[Response] = Applicative[F].pure(Response.notFound)
  override def post(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = Applicative[F].pure(Response.notImplemented)
  override def put(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = Applicative[F].pure(Response.notFound)
  override def patch(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = Applicative[F].pure(Response.notFound)
  override def delete(subPath: Path, queryParams: QueryParams): F[Response] = Applicative[F].pure(Response.notFound)
}
