package scim.rest

import cats.Applicative
import cats.effect.Sync
import cats.implicits._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.{Decoder, Json}
import scim.model.Codecs._
import scim.model.Filter.NoFilter
import scim.model._
import scim.spi.SpiError._
import scim.spi.{Sorting, UserStore}

private class UserResource[F[_]](urlConfig: UrlConfig)(implicit store: UserStore[F], applicative: Applicative[F], sync: Sync[F]) extends Resource[F] {
  private def pure[A]: A => F[A] = Applicative[F].pure

  override def get(subPath: Path, queryParams: QueryParams): F[Response] = subpathToId(subPath) match {
    case Some(id) =>
      store.get(id).map {
        case Right(user) =>
          Response.ok(user.asJson, locationHeader = Some(urlConfig.user(Some(id))))
        case Left(DoesNotExist(id)) =>
          Response.notFound(id)
      }

    case None =>
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
  }

  override def post(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = {
    if (subPath.isEmpty) {
      decodeBody[User](body)
        .map(create)
        .fold(pure, identity)
    } else if (subPath.headOption.contains(".search")) {
      if (subPath.size > 1) Applicative[F].pure(Response.notFound)
      decodeBody[SearchRequest](body)
        .map(query)
        .fold(pure, identity)
    } else {
      pure(Response.notFound)
    }
  }

  override def put(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = subpathToId(subPath) match {
    case Some(id) =>
      decodeBody[User](body)
        .flatMap(user => if (user.id.contains(id)) Right(user) else Left(Response.conflict("id mismatch between body and url")))
        .map(update)
        .fold(pure, identity)
    case None => pure(Response.notImplemented)
  }

  override def delete(subPath: Path, queryParams: QueryParams): F[Response] = subpathToId(subPath) match {
    case Some(id) =>
      store.delete(id).map {
        case Right(()) => Response.noContent
        case Left(DoesNotExist(id)) => Response.notFound(id)
      }
    case None => pure(Response.notImplemented)
  }

  override def patch(subPath: Path, queryParams: QueryParams, body: Json): F[Response] = subpathToId(subPath) match {
    case Some(id) =>
      decodeBody[PatchOp](body)
        .map(applyPatch(id))
        .fold(pure, identity)
    case None => pure(Response.notImplemented)
  }

  private def subpathToId(subPath: Path): Option[String] =
    Some(subPath.mkString("/")).filter(_.nonEmpty)

  private def decodeBody[A: Decoder](body: Json): Either[Response, A] =
    body.as[A].left.map(Response.decodingFailed)

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

  private def update(user: User): F[Response] = {
    store.update(user).map {
      case Right(updated) =>
        Response.ok(updated.asJson, locationHeader = Some(urlConfig.user(user.id)))
      case Left(DoesNotExist(id)) =>
        Response.notFound(id)
      case Left(Conflict(details)) =>
        Response.conflict(details)
    }
  }

  private def applyPatch(id: String)(op: PatchOp): F[Response] = {
    store.get(id).flatMap {
      case Right(current) =>
        op.applyTo(Schema.User)(current.json)
          .left.map(Response.decodingFailed)
          .map(User.apply)
          .map(update)
          .fold(pure, identity)
      case Left(DoesNotExist(id)) => pure(Response.notFound(id))
    }
  }
}
