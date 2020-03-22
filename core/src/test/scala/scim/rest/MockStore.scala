package scim.rest

import java.util.UUID
import cats.Id
import io.circe.Json.JString
import io.circe.{Decoder, Json}
import scim.model.Filter.{AttributePath, Comparison, StringValue}
import scim.model.{Codecs, ExtensibleModel, Filter, Group, Schema, User}
import scim.spi.{GroupStore, Paging, SearchResult, Sorting, UserStore}
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

object MockStore {
  trait MockUserStore extends UserStore[Id] with MockStore[User] {
    override protected def schema = Schema.User
    override protected def duplicate(a: User, b: User) = a.root.userName == b.root.userName
    override protected implicit def decoder: Decoder[User] = Codecs.userDecoder
  }
  trait MockGroupStore extends GroupStore[Id] with MockStore[Group] {
    override protected def schema = Schema.Group
    override protected def duplicate(a: Group, b: Group) = a.root.displayName == b.root.displayName
    override protected implicit def decoder: Decoder[Group] = Codecs.groupDecoder
  }
  trait MockOptimizedGroupStore extends GroupStore[Id] with MockStore[Group] {
    var wasOptimized = false
    override protected def schema = Schema.Group
    override protected def duplicate(a: Group, b: Group) = a.root.displayName == b.root.displayName
    override protected implicit def decoder: Decoder[Group] = Codecs.groupDecoder

    override def addToGroup(groupId: String, members: Set[Group.Member]) = Some {
      wasOptimized = true
      content.find(_.id.contains(groupId))
        .map { group =>
          val ms = members ++ group.root.members.getOrElse(Seq.empty)
          val g = Group(group.root.copy(members = Some(ms.toSeq)))
          content = content.filterNot(_.id.contains(groupId)) :+ g
          Right(())
        }.getOrElse(Left(DoesNotExist(groupId)))
    }

    override def removeFromGroup(groupId: String, filter: Filter) = filter match {
      case Comparison.Equal(AttributePath("value", None, None), StringValue(value)) =>
        wasOptimized = true
        Some(content.find(_.id.contains(groupId))
          .map { group =>
            val ms = group.root.members.getOrElse(Seq.empty).filterNot(_.value == value)
            val g = Group(group.root.copy(members = Some(ms.toSeq)))
            content = content.filterNot(_.id.contains(groupId)) :+ g
            Right(())
          }.getOrElse(Left(DoesNotExist(groupId))))
      case _ => None
    }
  }
}
