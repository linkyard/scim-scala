package scim.rest

import cats.effect.Sync
import cats.implicits._
import cats.{Applicative, Monad, MonadError}
import io.circe.Json
import io.circe.syntax._
import io.circe.generic.auto._
import scim.model.Codecs._
import scim.model.Filter.NoFilter
import scim.model.{Filter, ListResponse, SearchRequest, SortOrder}
import scim.spi.{Sorting, UserStore}

private class UserResource[F[_]](implicit store: UserStore[F], applicative: Applicative[F], sync: Sync[F]) extends Resource[F] {
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
      // handle creation
      // TODO
      Applicative[F].pure(Response.notImplemented)
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
    }.map(_.asJson)
      .map(body => Response(200, Some(body)))
  }

  // TODO
  override def put(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = Applicative[F].pure(Response.notImplemented)

  // TODO
  override def delete(subPath: Path, queryParams: QueryParams): F[Response] = Applicative[F].pure(Response.notImplemented)

  override def patch(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = Applicative[F].pure(Response.notImplemented)
}
