package scim.rest

import cats.Monad
import cats.implicits._
import io.circe.Json
import io.circe.generic.auto._
import io.circe.syntax._
import scim.model.Codecs._
import scim.model.Filter.NoFilter
import scim.model._
import scim.rest.Resource.{Path, QueryParams}
import scim.spi.SpiError._
import scim.spi.{GroupStore, Paging, Sorting}

private class GroupResource[F[_]](urlConfig: UrlConfig)(implicit store: GroupStore[F], monad: Monad[F]) extends Resource[F] {
  private def pure[A]: A => F[A] = monad.pure

  override def get(subPath: Path, queryParams: QueryParams): F[Response] = {
    Helpers.Get.retrieve(subPath, urlConfig.group)(store.get)
      .orElse(Helpers.Get.search(subPath, queryParams)(store.search))
      .getOrElse(pure(Response.notImplemented))
  }


  override def post(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = {
    Helpers.Post.create(subPath, body)(createInStore)
      .orElse(Helpers.Post.search(subPath, body)(store.search))
      .getOrElse(pure(Response.notImplemented))
  }

  override def put(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = {
    Helpers.Put.update(subPath, body)(updateInStore)
      .getOrElse(pure(Response.notImplemented))
  }

  override def delete(subPath: Path, queryParams: QueryParams): F[Response] = {
    Helpers.Delete.delete(subPath)(store.delete)
      .getOrElse(pure(Response.notImplemented))
  }


  override def patch(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = {
    Helpers.Patch.patchViaJson(subPath, body)(store.get, updateInStore, Schema.User)
      .getOrElse(pure(Response.notImplemented))
  }

  private def createInStore(group: Group): F[Response] = {
    store.create(group).map {
      case Right(created) =>
        Response.ok(created.asJson, locationHeader = Some(urlConfig.group(created.id)))
      case Left(AlreadyExists) =>
        Response.alreadyExists
      case Left(MalformedData(details)) =>
        Response.decodingFailed(details)
      case Left(MissingData(details)) =>
        Response.missingValue(details)
    }
  }

  private def updateInStore(group: Group): F[Response] = {
    store.update(group).map {
      case Right(updated) =>
        Response.ok(updated.asJson, locationHeader = Some(urlConfig.group(group.id)))
      case Left(DoesNotExist(id)) =>
        Response.notFound(id)
      case Left(Conflict(details)) =>
        Response.conflict(details)
    }
  }
}
