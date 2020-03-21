package scim.rest

import cats.Monad
import io.circe.Json
import io.circe.generic.auto._
import scim.model.Codecs._
import scim.model._
import scim.rest.Resource.{Path, QueryParams}
import scim.spi.UserStore

private class UserResource[F[_]](urlConfig: UrlConfig)(implicit store: UserStore[F], monad: Monad[F]) extends Resource[F] {
  private def pure[A]: A => F[A] = monad.pure

  override def get(subPath: Path, queryParams: QueryParams): F[Response] = {
    Helpers.Get.retrieve(subPath, urlConfig.user)(store.get)
      .orElse(Helpers.Get.search(subPath, queryParams)(store.search))
      .getOrElse(pure(Response.notImplemented))
  }


  override def post(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = {
    Helpers.Post.create(subPath, body, urlConfig.user)(store.create)
      .orElse(Helpers.Post.search(subPath, body)(store.search))
      .getOrElse(pure(Response.notImplemented))
  }

  override def put(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = {
    Helpers.Put.update(subPath, body, urlConfig.user)(store.update)
      .getOrElse(pure(Response.notImplemented))
  }

  override def delete(subPath: Path, queryParams: QueryParams): F[Response] = {
    Helpers.Delete.delete(subPath)(store.delete)
      .getOrElse(pure(Response.notImplemented))
  }


  override def patch(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = {
    Helpers.Patch.patchViaJson(subPath, body, urlConfig.user)(store.get, store.update, Schema.User)
      .getOrElse(pure(Response.notImplemented))
  }
}
