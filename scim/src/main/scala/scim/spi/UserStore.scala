package scim.spi

import scim.model.SearchRequest

trait UserStore[F[_]] {

//  def search(request: SearchRequest): Seq[User]

}

object UserStore {
  def apply[F[_]](implicit s: UserStore[F]): UserStore[F] = s
}
