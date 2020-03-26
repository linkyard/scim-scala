package scim.rest

import cats.Applicative
import io.circe.Json
import scim.model.ServiceProviderConfiguration
import scim.rest.Resource.{Path, QueryParams}
import scim.model.Codecs._
import io.circe.syntax._

private class ServiceProviderConfigResource[F[_]](config: ServiceProviderConfiguration)(implicit applicative: Applicative[F]) extends Resource[F] {
  private def pure[A]: A => F[A] = applicative.pure

  def get(subPath: Path, queryParams: QueryParams): F[Response] = pure(Response.ok(config.asJson))
  def post(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = pure(Response.notImplemented)
  def put(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = pure(Response.notImplemented)
  def patch(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = pure(Response.notImplemented)
  def delete(subPath: Path, queryParams: QueryParams): F[Response] = pure(Response.notImplemented)
}
