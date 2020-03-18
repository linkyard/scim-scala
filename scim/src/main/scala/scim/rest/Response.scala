package scim.rest

import io.circe.{DecodingFailure, Encoder, Json}
import io.circe.syntax._
import io.circe.generic.auto._
import scim.model.Error

case class Response(status: Int, body: Option[Json] = None, locationHeader: Option[String] = None)

object Response {
  def apply(status: Int, body: Json): Response = Response(status, Some(body))

  def error(error: Error): Response = Response(error.status, error.asJson)
  def notFound: Response = error(Error(404, detail = Some("Resource not found")))
  def notImplemented: Response = error(Error(501, detail = Some("Not implemented")))

  def decodingFailed(failure: DecodingFailure): Response = decodingFailed(failure.message)
  def decodingFailed(message: String): Response = error(Error(400,
    detail = Some(s"Parsing failed: $message"), scimType = Some("invalidValue")))
}
