package scim.rest

import java.net.URI

class UrlConfig protected(baseUrl: String) {
  def user(id: Option[String]): URI = URI.create(s"$baseUrl/Users/${id.map("/" + _).getOrElse("")}")
  def group(id: Option[String]): URI = URI.create(s"$baseUrl/Groups/${id.map("/" + _).getOrElse("")}")
}

object UrlConfig {
  def apply(baseUrl: String) = new UrlConfig(baseUrl.reverse.dropWhile(_ == '/').reverse)
}
