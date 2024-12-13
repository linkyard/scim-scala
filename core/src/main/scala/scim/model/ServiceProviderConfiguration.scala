package scim.model

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import scim.model.Codecs.given
import scim.model.ServiceProviderConfiguration.*

import java.net.URI

case class ServiceProviderConfiguration(
  patch: OptionSupported,
  bulk: BulkOptions,
  filter: FilterOptions,
  changePassword: OptionSupported,
  sort: OptionSupported,
  etag: OptionSupported,
  authenticationSchemes: Seq[AuthenticationOptions],
  documentationUri: Option[URI] = None,
  schemas: Seq[Schema] = Seq(Schema.ServiceProviderConfiguration),
) extends RootModel with JsonModel:
  override def asJson(base: URI): Json = Encoder[ServiceProviderConfiguration].apply(this)
    .deepMerge(Json.obj("meta" -> meta.resolveLocation(base).asJson))

  override def meta: Meta = Meta("ServiceProviderConfig", locationRelative = Some("/ServiceProviderConfig"))
end ServiceProviderConfiguration

object ServiceProviderConfiguration:
  case class OptionSupported(supported: Boolean)
  case class BulkOptions(supported: Boolean, maxOperations: Int, maxPayloadSize: Int)
  case class FilterOptions(supported: Boolean, maxResults: Int)
  case class AuthenticationOptions(
    `type`: String,
    name: String,
    description: String,
    specUri: Option[String] = None,
    documentationUri: Option[String] = None,
  )
