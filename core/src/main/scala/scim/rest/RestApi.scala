package scim.rest

import cats.Monad
import scim.model.ResourceType
import scim.model.SchemaDefinition
import scim.model.ServiceProviderConfiguration
import scim.model.ServiceProviderConfiguration.AuthenticationOptions
import scim.model.ServiceProviderConfiguration.BulkOptions
import scim.model.ServiceProviderConfiguration.FilterOptions
import scim.model.ServiceProviderConfiguration.OptionSupported
import scim.spi.GroupStore
import scim.spi.UserStore

trait RestApi[F[_]]:
  def user: Resource[F]
  def group: Resource[F]
  def resourceTypes: Resource[F]
  def schemas: Resource[F]
  def serviceProviderConfig: Resource[F]
  def me: Resource[F]
  def default: Resource[F]

object RestApi {
  def apply[F[_]: Monad: UserStore: GroupStore](
    urlConfig: UrlConfig,
    config: ServiceProviderConfiguration = defaultConfig,
    resourceTypes: Iterable[ResourceType] = defaultResourceTypes,
    schemaDefinitions: Iterable[SchemaDefinition] = SchemaDefinition.defaultSchemas,
  ): RestApi[F] =
    new Impl[F](config, urlConfig, resourceTypes, schemaDefinitions)

  private class Impl[F[_]](
    config: ServiceProviderConfiguration,
    urlConfig: UrlConfig,
    _resourceTypes: Iterable[ResourceType],
    schemaDefinitions: Iterable[SchemaDefinition],
  )(using Monad[F], UserStore[F], GroupStore[F]) extends RestApi[F]:
    def user: Resource[F] = new UserResource[F](urlConfig)
    def group: Resource[F] = new GroupResource[F](urlConfig)
    def resourceTypes: Resource[F] = new ResourceTypeResource[F](urlConfig, _resourceTypes)
    def schemas: Resource[F] = new SchemasResource(urlConfig, schemaDefinitions)
    def serviceProviderConfig: Resource[F] = new ServiceProviderConfigResource[F](urlConfig, config)
    def me: Resource[F] = new NotImplementedResource[F]
    def default: Resource[F] = new NotFoundResource[F]
  end Impl

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
    )),
  )

  val defaultResourceTypes: Iterable[ResourceType] = Seq(ResourceType.UserResourceType, ResourceType.GroupResourceType)
}
