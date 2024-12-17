package ch.linkyard.scim.rest

import cats.Id
import io.circe.Decoder
import io.circe.Json
import ch.linkyard.scim.model.Codecs
import ch.linkyard.scim.model.ExtensibleModel
import ch.linkyard.scim.model.Filter
import ch.linkyard.scim.model.Filter.AttributePath
import ch.linkyard.scim.model.Filter.Comparison
import ch.linkyard.scim.model.Filter.StringValue
import ch.linkyard.scim.model.Group
import ch.linkyard.scim.model.Schema
import ch.linkyard.scim.model.User
import ch.linkyard.scim.spi.GroupStore
import ch.linkyard.scim.spi.Paging
import ch.linkyard.scim.spi.SearchResult
import ch.linkyard.scim.spi.Sorting
import ch.linkyard.scim.spi.SpiError.*
import ch.linkyard.scim.spi.UserStore

import java.net.URI
import java.util.UUID

/** Mutable store in the Id monad */
trait MockStore[A <: ExtensibleModel[?]] {
  var content: Seq[A] = Seq.empty
  protected def schema: Schema
  protected def duplicate(a: A, b: A): Boolean
  protected implicit def decoder: Decoder[A]

  def get(id: String): Either[DoesNotExist, A] = content.find(_.id.contains(id)).toRight(DoesNotExist(id))

  def search(filter: Filter, paging: Paging, sorting: Option[Sorting]): SearchResult[A] = {
    val all = content.filter(u => filter.evaluate(u.asJson(URI.create("urn:none")), schema))
    val sorted = sorting.map(_.applyTo(all)).getOrElse(all)
    paging.applyTo(sorted)
  }

  def create(entity: A): Either[AlreadyExists.type, A] = {
    assert(entity.id.isEmpty)
    content.find(duplicate(_, entity)).map(_ => Left(AlreadyExists))
      .getOrElse {
        val withId = Json.fromJsonObject(entity.asJson(URI.create("urn:none")).asObject.get.add(
          "id",
          Json.fromString(UUID.randomUUID().toString),
        )).as[A]
          .getOrElse(throw new AssertionError("not reparsable to object"))
        content = content.appended(withId)
        Right(withId)
      }
  }

  def update(entity: A): Either[DoesNotExist, A] = {
    assert(entity.id.isDefined)
    content.find(_.id == entity.id)
      .map(_ => content = content.filterNot(_.id == entity.id).appended(entity))
      .map(_ => Right(entity))
      .getOrElse(Left(DoesNotExist(entity.id.getOrElse(""))))
  }

  def delete(id: String): Either[DoesNotExist, Unit] = {
    val after = content.filterNot(_.id.contains(id))
    if after.size == content.size then Left(DoesNotExist(id))
    else {
      content = after
      Right(())
    }
  }
}

object MockStore {
  trait MockUserStore extends UserStore[Id] with MockStore[User] {
    override protected def schema = Schema.User
    override protected def duplicate(a: User, b: User): Boolean = a.rootOrDefault.userName == b.rootOrDefault.userName
    override protected implicit def decoder: Decoder[User] = Codecs.given_Decoder_User
  }
  trait MockGroupStore extends GroupStore[Id] with MockStore[Group] {
    override protected def schema = Schema.Group
    override protected def duplicate(a: Group, b: Group): Boolean =
      a.rootOrDefault.displayName == b.rootOrDefault.displayName
    override protected implicit def decoder: Decoder[Group] = Codecs.given_Decoder_Group
  }
  trait MockOptimizedGroupStore extends GroupStore[Id] with MockStore[Group] {
    var wasOptimized = false
    override protected def schema = Schema.Group
    override protected def duplicate(a: Group, b: Group): Boolean =
      a.rootOrDefault.displayName == b.rootOrDefault.displayName
    override protected implicit def decoder: Decoder[Group] = Codecs.given_Decoder_Group

    override def addToGroup(groupId: String, members: Set[Group.Member]): Option[Id[Either[UpdateError, Unit]]] = Some {
      wasOptimized = true
      content.find(_.id.contains(groupId))
        .map { group =>
          val ms = members ++ group.rootOrDefault.members.getOrElse(Seq.empty)
          val g = Group(group.rootOrDefault.copy(members = Some(ms.toSeq)))
          content = content.filterNot(_.id.contains(groupId)) :+ g
          Right(())
        }.getOrElse(Left(DoesNotExist(groupId)))
    }

    override def removeFromGroup(groupId: String, filter: Filter): Option[Id[Either[UpdateError, Unit]]] =
      filter match {
        case Comparison.Equal(AttributePath("value", None, None), StringValue(value)) =>
          wasOptimized = true
          Some(content.find(_.id.contains(groupId))
            .map { group =>
              val ms = group.rootOrDefault.members.getOrElse(Seq.empty).filterNot(_.value == value)
              val g = Group(group.rootOrDefault.copy(members = Some(ms)))
              content = content.filterNot(_.id.contains(groupId)) :+ g
              Right(())
            }.getOrElse(Left(DoesNotExist(groupId))))
        case _ => None
      }
  }
}
