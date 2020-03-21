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
import scim.spi.{Paging, Sorting, UserStore}

private class UserResource[F[_]](urlConfig: UrlConfig)(implicit store: UserStore[F], monad: Monad[F]) extends Resource[F] {
  private def pure[A]: A => F[A] = monad.pure

  override def get(subPath: Path, queryParams: QueryParams): F[Response] = {
    Helpers.Get.retrieve(subPath)(retrieveFromStore)
      .orElse(Helpers.Get.search(subPath, queryParams)(queryStore))
      .getOrElse(pure(Response.notImplemented))
  }


  override def post(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = {
    Helpers.Post.create(subPath, body)(createInStore)
      .orElse(Helpers.Post.search(subPath, body)(queryStore))
      .getOrElse(pure(Response.notImplemented))
  }

  override def put(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = {
    Helpers.Put.update(subPath, body)(updateInStore)
      .getOrElse(pure(Response.notImplemented))
  }

  override def delete(subPath: Path, queryParams: QueryParams): F[Response] = {
    Helpers.Delete.delete(subPath)(deleteFromStore)
      .getOrElse(pure(Response.notImplemented))
  }


  override def patch(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = {
    Helpers.Patch.patchViaJson(subPath, body)(store.get, updateInStore, Schema.User)
      .getOrElse(pure(Response.notImplemented))
  }

  private def retrieveFromStore(id: String): F[Response] = {
    store.get(id).map {
      case Right(user) =>
        Response.ok(user.asJson, locationHeader = Some(urlConfig.user(Some(id))))
      case Left(DoesNotExist(id)) =>
        Response.notFound(id)
    }
  }

  private def queryStore(filter: Filter, paging: Paging, sorting: Option[Sorting]): F[Response] = {
    store.search(filter, paging, sorting).map { results =>
      ListResponse(
        totalResults = results.size,
        startIndex = Some(paging.start).filterNot(_ == 0).map(_ + 1),
        itemsPerPage = paging.maxResults,
        Resources = Some(results.map(_.asJson)).filter(_.nonEmpty)
      )
    }.map(body => Response.ok(body.asJson))
  }

  private def createInStore(user: User): F[Response] = {
    store.create(user).map {
      case Right(created) =>
        Response.ok(created.asJson, locationHeader = Some(urlConfig.user(created.id)))
      case Left(AlreadyExists) =>
        Response.alreadyExists
      case Left(MalformedData(details)) =>
        Response.decodingFailed(details)
      case Left(MissingData(details)) =>
        Response.missingValue(details)
    }
  }

  private def updateInStore(user: User): F[Response] = {
    store.update(user).map {
      case Right(updated) =>
        Response.ok(updated.asJson, locationHeader = Some(urlConfig.user(user.id)))
      case Left(DoesNotExist(id)) =>
        Response.notFound(id)
      case Left(Conflict(details)) =>
        Response.conflict(details)
    }
  }

  private def deleteFromStore(id: String): F[Response] = {
    store.delete(id).map {
      case Right(()) => Response.noContent
      case Left(DoesNotExist(id)) => Response.notFound(id)
    }
  }

}
