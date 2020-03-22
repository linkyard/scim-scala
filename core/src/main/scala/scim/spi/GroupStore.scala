package scim.spi

import scim.model.Group.Member
import scim.model.{Filter, Group}
import scim.spi.SpiError.{CreationError, DoesNotExist, UpdateError}

trait GroupStore[F[_]] {
  def get(id: String): F[Either[DoesNotExist, Group]]
  def search(filter: Filter, paging: Paging, sorting: Option[Sorting]): F[SearchResult[Group]]
  def create(group: Group): F[Either[CreationError, Group]]
  def update(group: Group): F[Either[UpdateError, Group]]
  def delete(id: String): F[Either[DoesNotExist, Unit]]

  /** Adds members to the group.
   * Optional function. Return None if not supported, in this case #update will be called instead. */
  def addToGroup(groupId: String, members: Set[Member]): Option[F[Either[UpdateError, Unit]]] = None

  /** Removes members from the group by applying a filter.
   * Optional function. Return None if not supported, in this case #update will be called instead.
   * It's ok to decline requests (return None) based on the filter provided */
  def removeFromGroup(groupId: String, filter: Filter): Option[F[Either[UpdateError, Unit]]] = None
}

object GroupStore {
  def apply[F[_]](implicit s: GroupStore[F]): GroupStore[F] = s
}
