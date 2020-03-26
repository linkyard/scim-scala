package scim.model

//import java.net.String
import java.net.URI
import scim.model.ServiceProviderConfiguration._

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
) extends RootModel

object ServiceProviderConfiguration {
  case class OptionSupported(supported: Boolean)
  case class BulkOptions(supported: Boolean, maxOperations: Int, maxPayloadSize: Int)
  case class FilterOptions(supported: Boolean, maxResults: Int)
  case class AuthenticationOptions(`type`: String, name: String, description: String, specUri: Option[String] = None, documentationUri: Option[String] = None)
}
