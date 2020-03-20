package scim.model

import java.net.URI
import java.util.Locale
import scala.util.Try
import io.circe.{Decoder, Encoder}
import scim.model.Filter.AttributeSelector
import scim.model.PatchOp.OperationType

object Codecs {
  implicit val localeDecoder: Decoder[Locale] = Decoder.decodeString.map(v => new Locale(v))
  implicit val localeEncoder: Encoder[Locale] = Encoder.encodeString.contramap(_.toString)
  implicit val uriDecoder: Decoder[URI] = Decoder.decodeString.emap(v => Try(URI.create(v)).toEither.left.map(_ => s"$v is not a valid URI"))
  implicit val uriEncoder: Encoder[URI] = Encoder.encodeString.contramap(_.toString)

  implicit val schemaDecoder: Decoder[Schema] = Decoder.decodeString.emap(Schema.parse)
  implicit val schemaEncoder: Encoder[Schema] = Encoder.encodeString.contramap(_.asString)

  implicit val filterDecoder: Decoder[Filter] = Decoder.decodeString.emap(Filter.parse)
  implicit val filterEncoder: Encoder[Filter] = Encoder.encodeString.contramap(_.render)

  implicit val attributeSelectorDecoder: Decoder[AttributeSelector] = Decoder.decodeString.emap(Filter.parseAttributeSelector)
  implicit val attributeSelectorEncoder: Encoder[AttributeSelector] = Encoder.encodeString.contramap(_.render)

  implicit val sortOrderDecoder: Decoder[SortOrder] = Decoder.decodeString.emap(SortOrder.parse)
  implicit val sortOrderEncoder: Encoder[SortOrder] = Encoder.encodeString.contramap(_.asString)

  implicit val operationTypeDecoder: Decoder[OperationType] = Decoder.decodeString.emap {
    case "add" => Right(OperationType.Add)
    case "remove" => Right(OperationType.Remove)
    case "replace" => Right(OperationType.Replace)
    case other => Left(s"Unsupported patch operation '$other'")
  }
  implicit val operationTypeEncoder: Encoder[OperationType] = Encoder.encodeString.contramap {
    case OperationType.Add => "add"
    case OperationType.Remove => "remove"
    case OperationType.Replace => "replace"
  }

  implicit val extensibleModelEncoder: Encoder[ExtensibleModel[_]] = v => v.asJson
  implicit val userDecoder: Decoder[User] = Decoder.decodeJson.map(User.apply)
}
