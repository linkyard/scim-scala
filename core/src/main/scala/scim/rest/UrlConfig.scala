package scim.rest

import java.net.URI

class UrlConfig(baseUrl: String) {
  def user(id: Option[String]): URI = URI.create(s"$baseUrl${id.map("/" + _).getOrElse("")}")
}
