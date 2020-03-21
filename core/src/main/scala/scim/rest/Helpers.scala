package scim.rest

import cats.{Applicative, Monad}
import scim.model.{ExtensibleModel, Filter, ListResponse, PatchOp, Schema, SearchRequest, SortOrder, User}
import scim.rest.Resource.{Path, QueryParams}
import cats.implicits._
import io.circe.{Decoder, Encoder, Json}
import io.circe.generic.auto._
import io.circe.syntax._
import scim.model.Codecs._
import scim.model.Filter.NoFilter
import scim.spi.{Paging, Sorting}
import scim.spi.SpiError.DoesNotExist

private object Helpers {
  type Id = String

  type QueryFun[F[_], A] = (Filter, Paging, Option[Sorting]) => F[Seq[A]]

  object Get {
    def retrieve[F[_]](subPath: Path)(get: Id => F[Response]): Option[F[Response]] = {
      subpathToId(subPath).map(get)
    }

    def search[F[_] : Applicative, A: Encoder](subPath: Path, queryParams: QueryParams)(query: QueryFun[F, A]): Option[F[Response]] = {
      if (subpathToId(subPath).isEmpty) Some {
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
          .map(executeQueryRequest(query))
          .fold(Applicative[F].pure, identity)
      } else None
    }
  }

  object Post {
    def create[F[_] : Applicative, A: Decoder](subPath: Path, body: Json)(create: A => F[Response]): Option[F[Response]] = {
      if (subPath.isEmpty) Some {
        decodeBody[A](body)
          .map(create)
          .fold(Applicative[F].pure, identity)
      } else None
    }

    def search[F[_] : Applicative, A: Encoder](subPath: Path, body: Json)(query: QueryFun[F, A]): Option[F[Response]] = {
      if (subPath.headOption.contains(".search")) Some {
        if (subPath.size > 1) Applicative[F].pure(Response.notFound)
        decodeBody[SearchRequest](body)
          .map(executeQueryRequest(query))
          .fold(Applicative[F].pure, identity)
      } else None
    }
  }

  object Put {
    def update[F[_] : Applicative, A <: ExtensibleModel[_] : Decoder](subPath: Path, body: Json)(update: A => F[Response]): Option[F[Response]] = {
      subpathToId(subPath).map { id =>
        decodeBody[A](body)
          .flatMap(entity => if (entity.id.contains(id)) Right(entity) else Left(Response.conflict("id mismatch between body and url")))
          .map(update)
          .fold(Applicative[F].pure, identity)
      }
    }
  }

  object Delete {
    def delete[F[_] : Applicative](subPath: Path)(delete: Id => F[Either[DoesNotExist, Unit]]): Option[F[Response]] = {
      subpathToId(subPath).map { id =>
        delete(id).map {
          case Right(()) => Response.noContent
          case Left(DoesNotExist(id)) => Response.notFound(id)
        }
      }
    }
  }

  object Patch {
    /** gets the current state, patches that and updates all fields */
    def patchViaJson[F[_] : Monad, E, A <: ExtensibleModel[_] : Decoder](subPath: Path, body: Json)(
      retrieve: Id => F[Either[DoesNotExist, A]], update: A => F[Response], schema: Schema): Option[F[Response]] = {
      subpathToId(subPath).map { id =>
        decodeBody[PatchOp](body)
          .map { op =>
            retrieve(id).flatMap {
              case Right(current) =>
                op.applyTo(current.schema)(current.json)
                  .left.map(Response.decodingFailed)
                  .flatMap(_.as[A].left.map(Response.decodingFailed))
                  .map(update)
                  .fold(Applicative[F].pure, identity)
              case Left(DoesNotExist(id)) =>
                Applicative[F].pure(Response.notFound(id))
            }
          }.fold(Applicative[F].pure, identity)
      }
    }
  }

  private def decodeBody[A: Decoder](body: Json): Either[Response, A] =
    body.as[A].left.map(Response.decodingFailed)

  private def subpathToId(subPath: Path): Option[Id] =
    Some(subPath.mkString("/")).filter(_.nonEmpty)

  private def executeQueryRequest[F[_] : Applicative, A: Encoder](query: QueryFun[F, A])(request: SearchRequest) = {
    val filter = request.filter.getOrElse(NoFilter)
    val paging = Paging(start = request.startIndex.map(_ - 1).map(_ max 0).getOrElse(0), maxResults = request.count.map(_ max 1))
    val sorting = request.sortBy.map(by => Sorting(by, request.sortOrder.getOrElse(SortOrder.default)))
    query(filter, paging, sorting)
      .map { results =>
        ListResponse(
          totalResults = results.size,
          startIndex = Some(paging.start).filterNot(_ == 0).map(_ + 1),
          itemsPerPage = paging.maxResults,
          Resources = Some(results.map(_.asJson)).filter(_.nonEmpty)
        )
      }.map(body => Response.ok(body.asJson))
  }
}
