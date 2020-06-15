package scim.rest

import java.net.URI

class UrlConfig protected(baseUrl: String) {
  val base: URI = URI.create(baseUrl)
  def user(id: Option[String]): URI = URI.create(s"$baseUrl/Users/${id.map("/" + _).getOrElse("")}")
  def group(id: Option[String]): URI = URI.create(s"$baseUrl/Groups/${id.map("/" + _).getOrElse("")}")
  def schema(name: Option[String]): URI = URI.create(s"$baseUrl/Schema/${name.map("/" + _).getOrElse("")}")
  def serviceProviderConfig: URI = URI.create(s"$baseUrl/ServiceProviderConfig")
}

object UrlConfig {
  def apply(baseUrl: String) = new UrlConfig(baseUrl.reverse.dropWhile(_ == '/').reverse)
}
