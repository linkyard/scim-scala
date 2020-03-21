package scim.rest

import java.util.UUID
import io.circe.{Decoder, Json}
import scim.model.{ExtensibleModel, Filter, Schema}
import scim.spi.{Paging, SearchResult, Sorting}
import scim.spi.SpiError._

/** Mutable store in the Id monad */
trait MockStore[A <: ExtensibleModel[_]] {
  var content: Seq[A] = Seq.empty
  protected def schema: Schema
  protected def duplicate(a: A, b: A): Boolean
  protected implicit def decoder: Decoder[A]
  def get(id: String) = content.find(_.id.contains(id)).toRight(DoesNotExist(id))
  def search(filter: Filter, paging: Paging, sorting: Option[Sorting]) = {
    val all = content.filter(u => filter.evaluate(u.asJson, schema))
    val page = all.slice(paging.start, paging.start + paging.maxResults.getOrElse(Int.MaxValue))
    SearchResult(page, all.size)
  }
  def create(entity: A) = {
    assert(entity.id.isEmpty)
    content.find(duplicate(_, entity)).map(_ => Left(AlreadyExists))
      .getOrElse {
        val withId = Json.fromJsonObject(entity.asJson.asObject.get.add("id", Json.fromString(UUID.randomUUID().toString))).as[A]
          .getOrElse(throw new AssertionError("not reparsable to object"))
        content = content.appended(withId)
        Right(withId)
      }
  }
  def update(entity: A) = {
    assert(entity.id.isDefined)
    content.find(_.id == entity.id)
      .map(_ => content = content.filterNot(_.id == entity.id).appended(entity))
      .map(_ => Right(entity))
      .getOrElse(Left(DoesNotExist(entity.id.getOrElse(""))))
  }

  def delete(id: String) = {
    val after = content.filterNot(_.id.contains(id))
    if (after.size == content.size) Left(DoesNotExist(id))
    else {
      content = after
      Right(())
    }
  }
}
