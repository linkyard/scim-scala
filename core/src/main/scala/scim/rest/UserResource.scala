package scim.rest

import cats.Monad
import cats.implicits.*
import io.circe.Json
import scim.model.*
import scim.model.Codecs.given
import scim.rest.Resource.Path
import scim.rest.Resource.QueryParams
import scim.spi.UserStore

private class UserResource[F[_]: Monad](urlConfig: UrlConfig)(using store: UserStore[F])
    extends Resource[F] {
  override def get(subPath: Path, queryParams: QueryParams): F[Response] =
    Helpers.Get.retrieve(subPath, urlConfig.base)(store.get)
      .orElse(Helpers.Get.search(subPath, queryParams, urlConfig.base)(store.search))
      .getOrElse(Response.notImplemented.pure)

  override def post(subPath: Path, queryParams: QueryParams, body: Json): F[Response] =
    Helpers.Post.create(subPath, body, urlConfig.base)(store.create)
      .orElse(Helpers.Post.search(subPath, body, urlConfig.base)(store.search))
      .getOrElse(Response.notImplemented.pure)

  override def put(subPath: Path, queryParams: QueryParams, body: Json): F[Response] =
    Helpers.Put.update(subPath, body, urlConfig.base)(store.update)
      .getOrElse(Response.notImplemented.pure)

  override def delete(subPath: Path, queryParams: QueryParams): F[Response] =
    Helpers.Delete.delete(subPath)(store.delete)
      .getOrElse(Response.notImplemented.pure)

  override def patch(subPath: Path, queryParams: QueryParams, body: Json): F[Response] =
    Helpers.Patch.patchViaJson(subPath, body, urlConfig.base)(store.get, store.update)
      .getOrElse(Response.notImplemented.pure)
}
