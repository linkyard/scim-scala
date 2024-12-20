package ch.linkyard.scim.model

import io.circe.DecodingFailure
import io.circe.Json

import java.net.URI

sealed trait Model

trait JsonModel extends Model:
  def asJson(base: URI): Json
  def meta: Meta

trait RootModel extends Model:
  def schemas: Seq[Schema]

trait ExtensibleModel[Root] extends Model with JsonModel:
  /** full/canonical representation */
  val json: Json

  def id: Option[String] = json.hcursor.downField("id").focus.flatMap(_.asString)
  def schema: Schema

  /** parsed representation */
  def root: Either[DecodingFailure, Root]
  def rootOrDefault: Root
