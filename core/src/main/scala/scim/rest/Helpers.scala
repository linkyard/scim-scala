package scim.rest

import java.net.URI
import cats.{Applicative, Monad}
import scim.model.{ExtensibleModel, Filter, JsonModel, ListResponse, Meta, PatchOp, Schema, SearchRequest, SortOrder, User}
import scim.rest.Resource.{Path, QueryParams}
import cats.implicits._
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._
import scim.model.Codecs._
import scim.model.Filter.Comparison.Equal
import scim.model.Filter.{AFilter, AttributePath, AttributeSelector, Comparison, FilteredAttributePath, NoFilter, StringValue}
import scim.model.PatchOp.OperationType
import scim.spi.{Paging, SearchResult, Sorting}
import scim.spi.SpiError.{AlreadyExists, Conflict, CreationError, DoesNotExist, MalformedData, MissingData, UpdateError}

private object Helpers {
  type Id = String

  type QueryFun[F[_], A] = (Filter, Paging, Option[Sorting]) => F[SearchResult[A]]

  object Get {
    def retrieve[F[_] : Applicative, A <: JsonModel](subPath: Path, base: URI)(get: Id => F[Either[DoesNotExist, A]]): Option[F[Response]] = {
      subpathToId(subPath).map { id =>
        get(id).map {
          case Right(r) =>
            Response.ok(r, base)
          case Left(DoesNotExist(id)) =>
            Response.notFound(id)
        }
      }
    }

    def search[F[_] : Applicative, A <: JsonModel](subPath: Path, queryParams: QueryParams, base: URI)(query: QueryFun[F, A]): Option[F[Response]] = {
      if subpathToId(subPath).isEmpty then Some {
        (for
          filter <- queryParams.get("filter").traverse(Filter.parse)
          sortBy = queryParams.get("sortBy").filter(_.nonEmpty)
          sortOrder <- queryParams.get("sortOrder").traverse(SortOrder.parse)
          startIndex <- queryParams.get("startIndex").traverse(IntValue.parse)
          count <- queryParams.get("count").traverse(IntValue.parse)
        yield SearchRequest(
          filter = filter,
          startIndex = startIndex,
          count = count,
          sortBy = sortBy,
          sortOrder = sortOrder,
          attributes = None,
          excludedAttributes = None,
        )).left.map(Response.malformedData)
          .map(executeQueryRequest(query, base))
          .fold(Applicative[F].pure, identity)
      } else None
    }
  }

  object Post {
    def create[F[_] : Applicative, A <: ExtensibleModel[_] : Decoder](subPath: Path, body: Json, base: URI)(
      create: A => F[Either[CreationError, A]]): Option[F[Response]] = {
      if subPath.isEmpty then Some {
        decodeBody[A](body)
          .map(create)
          .map(_.map {
            case Right(created) =>
              Response.ok(created, base)
            case Left(AlreadyExists) =>
              Response.alreadyExists
            case Left(MalformedData(details)) =>
              Response.malformedData(details)
            case Left(MissingData(details)) =>
              Response.missingValue(details)
          })
          .fold(Applicative[F].pure, identity)
      } else None
    }

    def search[F[_] : Applicative, A <: JsonModel](subPath: Path, body: Json, base: URI)(query: QueryFun[F, A]): Option[F[Response]] = {
      if subPath.headOption.contains(".search") then Some {
        if subPath.size > 1 then Applicative[F].pure(Response.notFound)
        else {
          decodeBody[SearchRequest](body)
            .map(executeQueryRequest(query, base))
            .fold(Applicative[F].pure, identity)
        }
      } else None
    }
  }

  object Put {
    def update[F[_] : Applicative, A <: ExtensibleModel[_] : Decoder](subPath: Path, body: Json, base: URI)(
      update: A => F[Either[UpdateError, A]]): Option[F[Response]] = {
      subpathToId(subPath).map { id =>
        decodeBody[A](body)
          .flatMap(entity =>
            if entity.id.isEmpty then addId(entity.json, id).as[A].left.map(Response.decodingFailed)
            else if entity.id.contains(id) then Right(entity)
            else Left(Response.conflict("id mismatch between body and url")))
          .map(update)
          .map(_.map(handleUpdateResult(base)))
          .fold(Applicative[F].pure, identity)
      }
    }

    private def addId(json: Json, id: String): Json = {
      json.asObject.map(_.add("id", Json.fromString(id)))
        .map(Json.fromJsonObject)
        .getOrElse(json)
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
    def patchViaJson[F[_] : Monad, E, A <: ExtensibleModel[_] : Decoder](subPath: Path, body: Json, base: URI)(
      retrieve: Id => F[Either[DoesNotExist, A]], update: A => F[Either[UpdateError, A]]): Option[F[Response]] = {
      subpathToId(subPath).map { id =>
        decodeBody[PatchOp](body)
          .map { op =>
            retrieve(id).flatMap {
              case Right(current) =>
                op.applyTo(current.schema)(current.json)
                  .left.map(Response.malformedData)
                  .flatMap(_.as[A].left.map(Response.decodingFailed))
                  .map(update)
                  .map(_.map(handleUpdateResult(base)))
                  .fold(Applicative[F].pure, identity)
              case Left(DoesNotExist(id)) =>
                Applicative[F].pure(Response.notFound(id))
            }
          }.fold(Applicative[F].pure, identity)
      }
    }

    type ArrayAddFun[F[_], M] = (String, Set[M]) => Option[F[Either[UpdateError, Unit]]]
    type ArrayRemoveFun[F[_]] = (String, Filter) => Option[F[Either[UpdateError, Unit]]]

    def patchArrayAttribute[F[_] : Applicative, M: Decoder](subPath: Path, body: Json, url: Option[Id] => URI)(
      attributeName: String, add: ArrayAddFun[F, M], remove: ArrayRemoveFun[F]): Option[F[Response]] = subpathToId(subPath).flatMap { id =>
      decodeBody[PatchOp](body).map { op =>
        def operationFor(op: PatchOp.Operation): Option[F[Either[UpdateError, Unit]]] = (op.path, op.op) match {
          case (Some(AttributePath(name, None, None)), OperationType.Add) if name == attributeName =>
            op.value.flatMap(_.asArray).map(_.traverse(_.as[M])).flatMap(_.toOption).map(_.toSet)
              .flatMap(add(id, _))

          case (Some(AttributePath(name, None, None)), OperationType.Remove) if name == attributeName =>
            op.value.flatMap(_.asArray)
              .filter(_.nonEmpty)
              .flatMap(_.traverse(_.asObject.filter(_.keys.toList == List("value")))) // only support if every value contains only 'value'
              .flatMap(_.traverse(_.apply("value").flatMap(_.asString))) // and 'value' must be a string
              .map(_.map(v => Equal(AttributePath("value", None, None), StringValue(v))))
              .map(_.reduceLeft[AFilter]((a, b) => Filter.Or(a, b)))
              .flatMap(remove(id, _))

          case (Some(FilteredAttributePath(name, filter, None, None)), OperationType.Remove) if name == attributeName =>
            remove(id, filter)

          case _ => None
        }


        op.Operations.map(operationFor).toList match {
          case Some(op) :: Nil => Some(op.map {
            case Right(()) => Response.noContent(url(Some(id)))
            case Left(DoesNotExist(id)) => Response.notFound(id)
            case Left(Conflict(details)) => Response.conflict(details)
            case Left(MalformedData(details)) => Response.malformedData(details)
            case Left(MissingData(details)) => Response.missingValue(details)
          })
          case None :: Nil => None // unsupported operation
          case a if a.length > 1 => None // multiple operations are not supported (since no transactions)
          case Nil => Some(Applicative[F].pure(Response.noContent(url(Some(id))))) // nothing to update..
        }
      }.fold(e => Some(Applicative[F].pure(e)), identity)
    }
  }

  private def handleUpdateResult[A <: ExtensibleModel[_]](base: URI)(updateResult: Either[UpdateError, A]): Response = {
    updateResult match {
      case Right(updated) => Response.ok(updated, base)
      case Left(DoesNotExist(id)) => Response.notFound(id)
      case Left(Conflict(details)) => Response.conflict(details)
      case Left(MalformedData(details)) => Response.malformedData(details)
      case Left(MissingData(details)) => Response.missingValue(details)
    }
  }

  private def decodeBody[A: Decoder](body: Json): Either[Response, A] =
    body.as[A].left.map(Response.decodingFailed)

  private def subpathToId(subPath: Path): Option[Id] =
    Some(subPath.mkString("/")).filter(_.nonEmpty)

  private def executeQueryRequest[F[_] : Applicative, A <: JsonModel](query: QueryFun[F, A], base: URI)(request: SearchRequest) = {
    val filter = request.filter.getOrElse(NoFilter)
    val paging = Paging(start = request.startIndex.map(_ - 1).map(_ max 0).getOrElse(0), maxResults = request.count.map(_ max 1))
    val sorting = request.sortBy.map(by => Sorting(by, request.sortOrder.getOrElse(SortOrder.default)))
    query(filter, paging, sorting)
      .map { result =>
        ListResponse(
          totalResults = result.totalCount,
          startIndex = Some(paging.start).filterNot(_ == 0).map(_ + 1),
          itemsPerPage = paging.maxResults,
          Resources = Some(result.results.map(_.asJson(base))).filter(_.nonEmpty)
        )
      }.map(body => Response.okJson(body.asJson))
  }
}
