package scim.rest

import io.circe.Decoder
import io.circe.Json
import io.circe.ParsingFailure
import org.scalatest.matchers.should.Matchers.*

object TestHelpers {
  implicit class ParserValues(r: Either[ParsingFailure, Json]) {
    def value: Json = r.getOrElse(fail(r.left.getOrElse(fail()).message))
  }

  implicit class DecoderValues[A](r: Decoder.Result[A]) {
    def value: A = r.getOrElse(fail(r.left.getOrElse(fail()).message))
  }
}
