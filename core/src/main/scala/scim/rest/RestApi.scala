package scim.rest

import cats.effect.Sync
import cats.{Monad, MonadError}
import scim.spi.UserStore

class RestApi[F[_]] private(implicit sync: Sync[F], userStore: UserStore[F]) {
  def user: Resource[F] = new UserResource[F]
  def group: Resource[F] = new GroupResource[F]
  def me: Resource[F] = new NotImplementedResource[F]
  def default: Resource[F] = new NotFoundResource[F]
}

object RestApi {
  def apply[F[_] : Sync : UserStore]: RestApi[F] = new RestApi[F]
}
