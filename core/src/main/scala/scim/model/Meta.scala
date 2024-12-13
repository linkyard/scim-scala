package scim.model

import java.net.URI
import java.time.Instant

case class Meta(
  resourceType: String,
  location: Option[URI] = None,
  locationRelative: Option[String] = None,
  created: Option[Instant] = None,
  lastModified: Option[Instant] = None,
  version: Option[String] = None,
):
  def resolveLocation(base: URI): Meta = copy(
    location = location.orElse(locationRelative.map(base.toString + _).map(URI.create)),
    locationRelative = None,
  )
