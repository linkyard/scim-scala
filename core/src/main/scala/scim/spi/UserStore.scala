package scim.spi

import scim.model.Filter
import scim.model.User
import scim.spi.SpiError.*

trait UserStore[F[_]]:
  def get(id: String): F[Either[DoesNotExist, User]]
  def search(filter: Filter, paging: Paging, sorting: Option[Sorting]): F[SearchResult[User]]
  def create(user: User): F[Either[CreationError, User]]
  def update(user: User): F[Either[UpdateError, User]]
  def delete(id: String): F[Either[DoesNotExist, Unit]]

object UserStore:
  def apply[F[_]](implicit s: UserStore[F]): UserStore[F] = s
