package scim.rest

import cats.Monad
import scim.model.ServiceProviderConfiguration
import scim.model.ServiceProviderConfiguration.{AuthenticationOptions, BulkOptions, FilterOptions, OptionSupported}
import scim.spi.{GroupStore, UserStore}

class RestApi[F[_]] private(config: ServiceProviderConfiguration, urlConfig: UrlConfig)(implicit monad: Monad[F], userStore: UserStore[F],
  groupStore: GroupStore[F]) {
  def user: Resource[F] = new UserResource[F](urlConfig)
  def group: Resource[F] = new GroupResource[F](urlConfig)
  def serviceProviderConfig: Resource[F] = new ServiceProviderConfigResource[F](config)
  def me: Resource[F] = new NotImplementedResource[F]
  def default: Resource[F] = new NotFoundResource[F]
}

object RestApi {
  def apply[F[_] : Monad : UserStore : GroupStore](urlConfig: UrlConfig, config: ServiceProviderConfiguration = defaultConfig): RestApi[F] = {
    new RestApi[F](config, urlConfig)
  }

  val defaultConfig = new ServiceProviderConfiguration(
    patch = OptionSupported(true),
    bulk = BulkOptions(supported = false, 0, 0),
    filter = FilterOptions(supported = true, 500),
    changePassword = OptionSupported(false),
    sort = OptionSupported(true),
    etag = OptionSupported(false),
    authenticationSchemes = Seq(AuthenticationOptions(
      `type` = "oauthbearertoken",
      name = "Bearer Token",
      description = "Bearer Token authentication in HTTP Authorization header",
    ))
  )
}
