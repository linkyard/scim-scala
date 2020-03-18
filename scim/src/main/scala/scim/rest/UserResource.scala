package scim.rest

import cats.implicits._
import cats.{Applicative, Monad}
import io.circe.Json
import io.circe.generic.auto._
import scim.json.Codecs._
import scim.model.{Filter, SearchRequest, SortOrder}
import scim.spi.UserStore

private class UserResource[F[_]](implicit store: UserStore[F], monad: Monad[F]) extends Resource[F] {
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
      ???
    }
  }

  override def post(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = {
    if (subPath.isEmpty) {
      // handle creation?
      ???
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
    ???
  }

  // TODO
  override def put(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = Applicative[F].pure(Response.notImplemented)

  // TODO
  override def delete(subPath: Path, queryParams: QueryParams): F[Response] = Applicative[F].pure(Response.notImplemented)

  override def patch(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = Applicative[F].pure(Response.notImplemented)
}
