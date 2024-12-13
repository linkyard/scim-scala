package scim.rest

import cats.Applicative
import io.circe.Json
import scim.model.ServiceProviderConfiguration
import scim.rest.Resource.Path
import scim.rest.Resource.QueryParams

private class ServiceProviderConfigResource[F[_]](urlConfig: UrlConfig, config: ServiceProviderConfiguration)(
  implicit applicative: Applicative[F]
) extends Resource[F] {
  private def pure[A]: A => F[A] = applicative.pure

  def get(subPath: Path, queryParams: QueryParams): F[Response] = pure {
    Response.okJson(config.asJson(urlConfig.base))
  }

  def post(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = pure(Response.notImplemented)
  def put(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = pure(Response.notImplemented)
  def patch(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = pure(Response.notImplemented)
  def delete(subPath: Path, queryParams: QueryParams): F[Response] = pure(Response.notImplemented)
}
