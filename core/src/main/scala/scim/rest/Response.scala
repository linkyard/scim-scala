package scim.rest

import java.net.URI
import io.circe.{DecodingFailure, Encoder, Json}
import io.circe.syntax._
import io.circe.generic.auto._
import scim.model.Error
import scim.model.Codecs._

case class Response(status: Int, body: Option[Json] = None, locationHeader: Option[URI] = None) {
  def headers: Map[String, String] = {
    locationHeader.map(v => Map("Location" -> v.toString)).toSeq
      .fold(Response.defaultHeaders)(_ ++ _)
  }
}

object Response {
  def apply(status: Int, body: Json): Response = Response(status, Some(body))

  private val defaultHeaders = Map("Content-Type" -> "application/scim+json")

  def ok: Response = Response(200, None)
  def ok(body: Json, locationHeader: Option[URI] = None): Response = Response(200, Some(body), locationHeader = locationHeader)
  def noContent: Response = Response(204, None)

  def error(error: Error): Response = Response(error.status, error.asJson)

  def decodingFailed(failure: DecodingFailure): Response = decodingFailed(failure.message)
  def decodingFailed(message: String): Response = error(Error(400,
    detail = Some(s"Parsing failed: $message"), scimType = Some("invalidValue")))
  def missingValue(detail: String): Response = error(Error(400,
    detail = Some(s"Missing value: $detail"), scimType = Some("invalidValue")))
  def notFound: Response = error(Error(404, detail = Some("Resource not found")))
  def alreadyExists: Response = error(Error(409, detail = Some("Already exists"), scimType = Some("uniqueness")))

  def notImplemented: Response = error(Error(501, detail = Some("Not implemented")))
}
