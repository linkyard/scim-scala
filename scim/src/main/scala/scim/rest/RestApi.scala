package scim.rest

import cats.Monad
import scim.spi.UserStore

class RestApi[F[_]] private(implicit monad: Monad[F], userStore: UserStore[F]) {
  def user: Resource[F] = new UserResource[F]
  def group: Resource[F] = new GroupResource[F]
  def me: Resource[F] = new NotImplementedResource[F]
  def default: Resource[F] = new NotFoundResource[F]
}

object RestApi {
  def apply[F[_]: Monad: UserStore]: RestApi[F] = new RestApi[F]
}
