package scim.model

trait Filter {

  def render: String = ???
  def asString: String = render
}
object Filter {
  def parse(string: String): Either[String, Filter] = ???
}
