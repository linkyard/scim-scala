package scim.rest

import cats.Applicative
import cats.implicits.*
import io.circe.Json
import scim.model.ServiceProviderConfiguration
import scim.rest.Resource.Path
import scim.rest.Resource.QueryParams

private class ServiceProviderConfigResource[F[_]: Applicative](
  urlConfig: UrlConfig,
  config: ServiceProviderConfiguration,
) extends Resource[F]:
  def get(subPath: Path, queryParams: QueryParams): F[Response] =
    Response.okJson(config.asJson(urlConfig.base)).pure

  def post(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = Response.notImplemented.pure
  def put(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = Response.notImplemented.pure
  def patch(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = Response.notImplemented.pure
  def delete(subPath: Path, queryParams: QueryParams): F[Response] = Response.notImplemented.pure
