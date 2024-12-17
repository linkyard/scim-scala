package ch.linkyard.scim.rest

import cats.Monad
import ch.linkyard.scim.model.Codecs.given
import ch.linkyard.scim.model.Group.Member
import ch.linkyard.scim.rest.Resource.Path
import ch.linkyard.scim.rest.Resource.QueryParams
import io.circe.Json
import scim.spi.GroupStore

private class GroupResource[F[_]: Monad](urlConfig: UrlConfig)(using store: GroupStore[F])
    extends Resource[F]:
  private def pure[A]: A => F[A] = Monad[F].pure

  override def get(subPath: Path, queryParams: QueryParams): F[Response] =
    Helpers.Get.retrieve(subPath, urlConfig.base)(store.get)
      .orElse(Helpers.Get.search(subPath, queryParams, urlConfig.base)(store.search))
      .getOrElse(pure(Response.notImplemented))

  override def post(subPath: Path, queryParams: QueryParams, body: Json): F[Response] =
    Helpers.Post.create(subPath, body, urlConfig.base)(store.create)
      .orElse(Helpers.Post.search(subPath, body, urlConfig.base)(store.search))
      .getOrElse(pure(Response.notImplemented))

  override def put(subPath: Path, queryParams: QueryParams, body: Json): F[Response] =
    Helpers.Put.update(subPath, body, urlConfig.base)(store.update)
      .getOrElse(pure(Response.notImplemented))

  override def delete(subPath: Path, queryParams: QueryParams): F[Response] =
    Helpers.Delete.delete(subPath)(store.delete)
      .getOrElse(pure(Response.notImplemented))

  override def patch(subPath: Path, queryParams: QueryParams, body: Json): F[Response] =
    Helpers.Patch.patchArrayAttribute[F, Member](subPath, body, urlConfig.group)(
      "members",
      store.addToGroup,
      store.removeFromGroup,
    )
      .orElse(Helpers.Patch.patchViaJson(subPath, body, urlConfig.base)(store.get, store.update))
      .getOrElse(pure(Response.notImplemented))
