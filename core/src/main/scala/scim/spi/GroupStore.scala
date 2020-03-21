package scim.spi

import scim.model.{Filter, Group}
import scim.spi.SpiError.{CreationError, DoesNotExist, UpdateError}

trait GroupStore[F[_]] {
  def get(id: String): F[Either[DoesNotExist, Group]]
  def search(filter: Filter, paging: Paging, sorting: Option[Sorting]): F[SearchResult[Group]]
  def create(group: Group): F[Either[CreationError, Group]]
  def update(group: Group): F[Either[UpdateError, Group]]
  def delete(id: String): F[Either[DoesNotExist, Unit]]
}

object GroupStore {
  def apply[F[_]](implicit s: GroupStore[F]): GroupStore[F] = s
}
