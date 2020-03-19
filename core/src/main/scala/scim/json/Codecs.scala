package scim.json

import io.circe.{Decoder, Encoder}
import scim.model.{Filter, Schema, SortOrder}

object Codecs {
  implicit val schemaDecoder: Decoder[Schema] = Decoder.decodeString.emap(Schema.parse)
  implicit val schemaEncoder: Encoder[Schema] = Encoder.encodeString.contramap(_.asString)

  implicit val filterDecoder: Decoder[Filter] = Decoder.decodeString.emap(Filter.parse)
  implicit val filterEncoder: Encoder[Filter] = Encoder.encodeString.contramap(_.render)

  implicit val sortOrderDecoder: Decoder[SortOrder] = Decoder.decodeString.emap(SortOrder.parse)
  implicit val sortOrderEncoder: Encoder[SortOrder] = Encoder.encodeString.contramap(_.asString)
}
