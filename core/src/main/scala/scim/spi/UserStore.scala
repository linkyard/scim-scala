package scim.spi

import scim.model.{Filter, SearchRequest, User}
import fs2.Stream
import SpiError._

trait UserStore[F[_]] {
  def create(user: User): F[Either[CreationError, User]]
  def search(filter: Filter, sorting: Option[Sorting]): Stream[F, User]
}

object UserStore {
  def apply[F[_]](implicit s: UserStore[F]): UserStore[F] = s
}
