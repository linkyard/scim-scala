package ch.linkyard.scim.rest

import ch.linkyard.scim.model.Codecs.given
import ch.linkyard.scim.model.Error
import ch.linkyard.scim.model.JsonModel
import io.circe.DecodingFailure
import io.circe.Json
import io.circe.syntax.*

import java.net.URI

case class Response(status: Int, body: Option[Json] = None, locationHeader: Option[URI] = None)

object Response:
  def apply(status: Int, body: Json): Response = Response(status, Some(body.deepDropNullValues))

  def ok: Response = Response(200, None)
  def ok(body: JsonModel, base: URI): Response =
    okJson(body.asJson(base), locationHeader = body.meta.resolveLocation(base).location)
  def okJson(body: Json, locationHeader: Option[URI] = None): Response =
    Response(200, Some(body.deepDropNullValues), locationHeader = locationHeader)
  def noContent: Response = Response(204, None)
  def noContent(locationHeader: URI): Response = Response(204, None, locationHeader = Some(locationHeader))

  def error(error: Error): Response = Response(error.status, error.asJson)

  def decodingFailed(failure: DecodingFailure): Response = malformedData(failure.message)
  def malformedData(message: String): Response =
    error(Error(400, detail = Some(s"Parsing failed: $message"), scimType = Some("invalidValue")))
  def missingValue(detail: String): Response =
    error(Error(400, detail = Some(s"Missing value: $detail"), scimType = Some("invalidValue")))
  def forbidden: Response = forbidden("not allowed")
  def forbidden(detail: String): Response =
    error(Error(403, detail = Some(detail)))
  def notFound: Response =
    error(Error(404, detail = Some("Resource not found")))
  def notFound(id: String): Response =
    error(Error(404, detail = Some(s"Resource with id $id not found")))
  def alreadyExists: Response = error(Error(409, detail = Some("Already exists"), scimType = Some("uniqueness")))
  def conflict(details: String): Response = error(Error(409, detail = Some(details), scimType = Some("mutability")))

  def notImplemented: Response = error(Error(501, detail = Some("Not implemented")))
