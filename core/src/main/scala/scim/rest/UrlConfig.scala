package scim.rest

import java.net.URI

class UrlConfig protected (baseUrl: String) {
  val base: URI = URI.create(baseUrl)
  def group(id: Option[String]): URI = URI.create(s"$baseUrl/Groups/${id.map("/" + _).getOrElse("")}")
}

object UrlConfig {
  def apply(baseUrl: String) = new UrlConfig(baseUrl.reverse.dropWhile(_ == '/').reverse)
}
