package scim.model

import java.net.URI
import io.circe.{Encoder, Json}
import io.circe.syntax._
import io.circe.generic.auto._
import scim.model.ServiceProviderConfiguration._
import Codecs._

case class ServiceProviderConfiguration(
  patch: OptionSupported,
  bulk: BulkOptions,
  filter: FilterOptions,
  changePassword: OptionSupported,
  sort: OptionSupported,
  etag: OptionSupported,
  authenticationSchemes: Seq[AuthenticationOptions],
  documentationUri: Option[URI] = None,
  schemas: Seq[Schema] = Seq(Schema.ServiceProviderConfiguration)
) extends RootModel with JsonModel {
  override def asJson(base: URI): Json = {
    val encoder = implicitly[Encoder[ServiceProviderConfiguration]]
    encoder(this).deepMerge(Json.obj("meta" -> meta.resolveLocation(base).asJson))
  }
  override def meta: Meta = Meta("ServiceProviderConfig", locationRelative = Some("/ServiceProviderConfig"))
}

object ServiceProviderConfiguration {
  case class OptionSupported(supported: Boolean)
  case class BulkOptions(supported: Boolean, maxOperations: Int, maxPayloadSize: Int)
  case class FilterOptions(supported: Boolean, maxResults: Int)
  case class AuthenticationOptions(`type`: String, name: String, description: String, specUri: Option[String] = None, documentationUri: Option[String] = None)
}
