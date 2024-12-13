package scim.model

import io.circe.Codec
import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.*
import scim.model.Filter.AttributeSelector
import scim.model.PatchOp.OperationType
import scim.model.ResourceType.SchemaExtension
import scim.model.ServiceProviderConfiguration.AuthenticationOptions
import scim.model.ServiceProviderConfiguration.BulkOptions
import scim.model.ServiceProviderConfiguration.FilterOptions
import scim.model.ServiceProviderConfiguration.OptionSupported

import java.net.URI
import java.util.Locale
import scala.util.Try

object Codecs {
  given Decoder[Locale] = Decoder.decodeString.map(v => new Locale(v))
  given Encoder[Locale] = Encoder.encodeString.contramap(_.toString)
  given Decoder[URI] =
    Decoder.decodeString.emap(v => Try(URI.create(v)).toEither.left.map(_ => s"$v is not a valid URI"))
  given Encoder[URI] = Encoder.encodeString.contramap(_.toString)

  given Decoder[Schema] = Decoder.decodeString.emap(Schema.parse)
  given Encoder[Schema] = Encoder.encodeString.contramap(_.asString)

  given Decoder[Filter] = Decoder.decodeString.emap(Filter.parse)
  given Encoder[Filter] = Encoder.encodeString.contramap(_.render)

  given Decoder[AttributeSelector] =
    Decoder.decodeString.emap(Filter.parseAttributeSelector)
  given Encoder[AttributeSelector] = Encoder.encodeString.contramap(_.render)

  given Decoder[SortOrder] = Decoder.decodeString.emap(SortOrder.parse)
  given Encoder[SortOrder] = Encoder.encodeString.contramap(_.asString)

  given Decoder[OperationType] = Decoder.decodeString.map(_.toLowerCase).emap {
    case "add"     => Right(OperationType.Add)
    case "remove"  => Right(OperationType.Remove)
    case "replace" => Right(OperationType.Replace)
    case other     => Left(s"Unsupported patch operation '$other'")
  }
  given Encoder[OperationType] = Encoder.encodeString.contramap {
    case OperationType.Add     => "add"
    case OperationType.Remove  => "remove"
    case OperationType.Replace => "replace"
  }
  given Codec[PatchOp.Operation] = deriveCodec
  given Codec[PatchOp] = deriveCodec
  given Codec[OptionSupported] = deriveCodec
  given Codec[BulkOptions] = deriveCodec
  given Codec[FilterOptions] = deriveCodec
  given Codec[AuthenticationOptions] = deriveCodec

  given Codec[Error] = deriveCodec
  given Codec[SearchRequest] = deriveCodec
  given Codec[ListResponse] = deriveCodec
  given Codec[ServiceProviderConfiguration] = deriveCodec

  given Decoder[User] = Decoder.decodeJson.map(User.apply)
  given Decoder[Group] = Decoder.decodeJson.map(Group.apply)
  given Codec[Group.Member] = deriveCodec

  given Codec[SchemaExtension] = deriveCodec
  given Codec[ResourceType] = deriveCodec

  given Codec[Meta] = deriveCodec
}
