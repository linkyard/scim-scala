package scim.json

import io.circe.{Decoder, Encoder}
import scim.model.{Filter, SortOrder}

object Codecs {
  implicit val filterDecoder: Decoder[Filter] = Decoder.decodeString.emap(Filter.parse)
  implicit val filterEncoder: Encoder[Filter] = Encoder.encodeString.contramap(_.render)

  implicit val sortOrderDecoder: Decoder[SortOrder] = Decoder.decodeString.emap(SortOrder.parse)
  implicit val sortOrderEncoder: Encoder[SortOrder] = Encoder.encodeString.contramap(_.asString)
}
