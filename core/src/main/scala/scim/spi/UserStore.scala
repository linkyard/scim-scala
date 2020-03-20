package scim.spi

import fs2.Stream
import scim.model.{Filter, User}
import scim.spi.SpiError._

trait UserStore[F[_]] {
  def get(id: String): F[Either[DoesNotExist, User]]
  def search(filter: Filter, sorting: Option[Sorting]): Stream[F, User]
  def create(user: User): F[Either[CreationError, User]]
  def update(user: User): F[Either[UpdateError, User]]
  def delete(id: String): F[Either[DoesNotExist, Unit]]
}

object UserStore {
  def apply[F[_]](implicit s: UserStore[F]): UserStore[F] = s
}
