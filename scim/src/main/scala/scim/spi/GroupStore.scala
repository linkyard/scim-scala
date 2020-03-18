package scim.spi

trait GroupStore[F[_]] {

}

object GroupStore {
  def apply[F[_]](implicit s: GroupStore[F]): GroupStore[F] = s
}
