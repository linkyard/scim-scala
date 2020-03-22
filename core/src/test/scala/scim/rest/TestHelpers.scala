package scim.rest

import io.circe.{Decoder, Json, ParsingFailure}
import org.scalatest.matchers.should.Matchers._

object TestHelpers extends {
  implicit class ParserValues(r: Either[ParsingFailure, Json]) {
    def value: Json = r.getOrElse(fail(r.left.getOrElse(fail()).message))
  }

  implicit class DecoderValues[A](r: Decoder.Result[A]) {
    def value: A = r.getOrElse(fail(r.left.getOrElse(fail()).message))
  }
}
