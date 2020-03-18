package scim.spi

import scim.model.{Filter, SearchRequest, User}

trait UserStore[F[_]] {

  def search(filter: Filter, sorting: Option[Sorting]): Seq[User]

}

object UserStore {
  def apply[F[_]](implicit s: UserStore[F]): UserStore[F] = s
}
