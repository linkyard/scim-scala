package scim

import scala.util.Try

package object rest {
  object IntValue {
    def parse(string: String): Either[String, Int] = {
      Try(Integer.parseInt(string)).toEither.left.map(_ => s"'$string' is not a number")
    }
  }
}
