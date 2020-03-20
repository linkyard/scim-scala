package scim.rest

import cats.effect.Sync
import cats.implicits._
import cats.{Applicative, Monad, MonadError}
import io.circe.Json
import io.circe.syntax._
import io.circe.generic.auto._
import scim.model.Codecs._
import scim.model.Filter.NoFilter
import scim.model.{Error, Filter, ListResponse, SearchRequest, SortOrder, User}
import scim.spi.SpiError.{AlreadyExists, MalformedData, MissingData}
import scim.spi.{Sorting, UserStore}

private class UserResource[F[_]](urlConfig: UrlConfig)(implicit store: UserStore[F], applicative: Applicative[F], sync: Sync[F]) extends Resource[F] {
  private def pure[A]: A => F[A] = Applicative[F].pure

  override def get(subPath: Path, queryParams: QueryParams): F[Response] = {
    if (subPath.isEmpty) {
      (for {
        filter <- queryParams.get("filter").traverse(Filter.parse)
        sortBy = queryParams.get("sortBy").filter(_.nonEmpty)
        sortOrder <- queryParams.get("sortOrder").traverse(SortOrder.parse)
        startIndex <- queryParams.get("startIndex").traverse(IntValue.parse)
        count <- queryParams.get("count").traverse(IntValue.parse)
      } yield SearchRequest(
        filter = filter,
        startIndex = startIndex,
        count = count,
        sortBy = sortBy,
        sortOrder = sortOrder,
        attributes = None,
        excludedAttributes = None,
      )).left.map(Response.decodingFailed)
        .map(query)
        .fold(pure, identity)
    } else {
      // get a specific user
      // TODO
      Applicative[F].pure(Response.notImplemented)
    }
  }

  override def post(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = {
    if (subPath.isEmpty) {
      body.as[User]
        .left.map(Response.decodingFailed)
        .map(create)
        .fold(pure, identity)
    } else if (subPath.headOption.contains(".search")) {
      if (subPath.size > 1) Applicative[F].pure(Response.notFound)
      body.as[SearchRequest]
        .left.map(Response.decodingFailed)
        .map(query)
        .fold(pure, identity)
    } else {
      pure(Response.notFound)
    }
  }

  // TODO
  override def put(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = Applicative[F].pure(Response.notImplemented)

  // TODO
  override def delete(subPath: Path, queryParams: QueryParams): F[Response] = Applicative[F].pure(Response.notImplemented)

  override def patch(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = Applicative[F].pure(Response.notImplemented)


  private def query(request: SearchRequest): F[Response] = {
    val sorting = request.sortBy.map(by => Sorting(by, request.sortOrder.getOrElse(SortOrder.default)))
    val stream = store.search(request.filter.getOrElse(NoFilter), sorting)
      .drop(request.startIndex.map(i => (i - 1) max 0).getOrElse(0).toLong)
    stream.compile.toVector.map { users =>
      val result = request.count.map(users.take).getOrElse(users)
        .map(_.asJson)
      ListResponse(
        totalResults = users.size,
        startIndex = request.startIndex,
        itemsPerPage = request.count,
        Resources = Some(result).filter(_.nonEmpty)
      )
    }.map(body => Response.ok(body.asJson))
  }

  private def create(user: User): F[Response] = {
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
}
