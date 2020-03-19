package scim.model

import java.net.URI
import java.util.Locale
import scala.util.Try
import io.circe.{Decoder, Encoder}

object Codecs {
  implicit val localeDecoder: Decoder[Locale] = Decoder.decodeString.map(v => new Locale(v))
  implicit val localeEncoder: Encoder[Locale] = Encoder.encodeString.contramap(_.toString)
  implicit val uriDecoder: Decoder[URI] = Decoder.decodeString.emap(v => Try(URI.create(v)).toEither.left.map(_ => s"$v is not a valid URI"))
  implicit val uriEncoder: Encoder[URI] = Encoder.encodeString.contramap(_.toString)

  implicit val schemaDecoder: Decoder[Schema] = Decoder.decodeString.emap(Schema.parse)
  implicit val schemaEncoder: Encoder[Schema] = Encoder.encodeString.contramap(_.asString)

  implicit val filterDecoder: Decoder[Filter] = Decoder.decodeString.emap(Filter.parse)
  implicit val filterEncoder: Encoder[Filter] = Encoder.encodeString.contramap(_.render)

  implicit val sortOrderDecoder: Decoder[SortOrder] = Decoder.decodeString.emap(SortOrder.parse)
  implicit val sortOrderEncoder: Encoder[SortOrder] = Encoder.encodeString.contramap(_.asString)

  implicit val extensibleModelEncoder: Encoder[ExtensibleModel[_]] = v => v.asJson
  implicit val userDecoder: Decoder[User] = Decoder.decodeJson.map(User.apply)
}
