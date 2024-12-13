package scim.rest

object IntValue:
  def parse(string: String): Either[String, Int] =
    string.toIntOption.toRight(s"'$string' is not a number")
