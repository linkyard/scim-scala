package scim.model

import java.net.URI
import scala.util.Try
import fastparse.internal.Logger
import io.circe.Json

/** RFC 7644 - 3.4.2.2. */
sealed trait Filter {
  def render: String
  def asString: String = render
}

object Filter {
  def parse(string: String): Either[String, Filter] = Parser(string)

  def none: Filter = NoFilter

  sealed trait AFilter extends Filter
  case object NoFilter extends Filter {
    def render = ""
  }

  sealed trait LogicalOperation extends AFilter
  case class And(a: AFilter, b: AFilter) extends LogicalOperation {
    def render = s"${a.render} and ${b.render}"
  }
  case class Or(a: AFilter, b: AFilter) extends LogicalOperation {
    def render = s"${a.render} or ${b.render}"
  }
  case class Not(a: AFilter) extends LogicalOperation {
    def render = s"not (${a.render})"
  }

  sealed trait Comparison extends AFilter
  object Comparison {
    private def renderJson(json: Json): String = json.spaces2

    case class Equal(attributePath: AttributePath, value: Json) extends Comparison {
      def render = s"${attributePath.render} eq ${renderJson(value)}"
    }
    case class NotEqual(attributePath: AttributePath, value: Json) extends Comparison {
      def render = s"${attributePath.render} ne ${renderJson(value)}"
    }
    case class Contains(attributePath: AttributePath, value: Json) extends Comparison {
      def render = s"${attributePath.render} co ${renderJson(value)}"
    }
    case class StartsWith(attributePath: AttributePath, value: Json) extends Comparison {
      def render = s"${attributePath.render} sw ${renderJson(value)}"
    }
    case class EndsWith(attributePath: AttributePath, value: Json) extends Comparison {
      def render = s"${attributePath.render} ew ${renderJson(value)}"
    }
    case class Present(attributePath: AttributePath) extends Comparison {
      def render = s"${attributePath.render} pr"
    }
    case class GreaterThan(attributePath: AttributePath, value: Json) extends Comparison {
      def render = s"${attributePath.render} gt ${renderJson(value)}"
    }
    case class GreaterThanOrEqual(attributePath: AttributePath, value: Json) extends Comparison {
      def render = s"${attributePath.render} ge ${renderJson(value)}"
    }
    case class LessThan(attributePath: AttributePath, value: Json) extends Comparison {
      def render = s"${attributePath.render} lt ${renderJson(value)}"
    }
    case class LessThanOrEqual(attributePath: AttributePath, value: Json) extends Comparison {
      def render = s"${attributePath.render} le ${renderJson(value)}"
    }
  }

  case class ComplexAttributeFilter(attributePath: AttributePath, filter: AFilter) extends AFilter {
    def render = s"${attributePath.render}[${filter.render}]"
  }

  case class AttributePath(name: String, uri: Option[URI] = None, subAttribute: Option[String] = None) {
    def render: String = uri.map(_.toString + ":").getOrElse("") + name + subAttribute.map("." + _).getOrElse("")
  }


  private object Parser {
    import fastparse._, NoWhitespace._

    // TODO remove
    implicit val logger: Logger = Logger.stdout

    def apply(string: String): Either[String, Filter] = {
      fastparse.parse(string, completeFilter(_), verboseFailures = true) match {
        case Parsed.Success(value, index) => Right(value)
        case failure: Parsed.Failure => Left(failure.longMsg)
      }
    }

    /**
     * FILTER    = attrExp / logExp / valuePath / *1"not" "(" FILTER ")"
     *
     * valuePath = attrPath "[" valFilter "]"
     * ; FILTER uses sub-attributes of a parent attrPath
     *
     * valFilter = attrExp / logExp / *1"not" "(" valFilter ")"
     *
     * attrExp   = (attrPath SP "pr") /
     * (attrPath SP compareOp SP compValue)
     *
     * logExp    = FILTER SP ("and" / "or") SP FILTER
     *
     * compValue = false / null / true / number / string
     * ; rules from JSON (RFC 7159)
     *
     * compareOp = "eq" / "ne" / "co" /
     * "sw" / "ew" /
     * "gt" / "lt" /
     * "ge" / "le"
     *
     * attrPath  = [URI ":"] ATTRNAME *1subAttr
     * ; SCIM attribute name
     * ; URI is SCIM "schema" URI
     *
     * ATTRNAME  = ALPHA *(nameChar)
     *
     * nameChar  = "-" / "_" / DIGIT / ALPHA
     *
     * subAttr   = "." ATTRNAME
     * ; a sub-attribute of a complex attribute
     *
     * Figure 1: ABNF Specification of SCIM Filters
     */

    def alpha[_: P] = P(CharIn("a-zA-Z").!)
    def digit[_: P] = P(CharIn("0-9").!)
    def uriPart[_: P] = P(CharsWhileIn("a-zA-Z0-9\\-\\.", 1))
    def uriPrefix[_: P] = P("urn" ~ (":" ~ uriPart ~ &(":")).rep(1)).!
      .map(v => Try(URI.create(v)).toEither).filter(_.isRight)
      .map(_.getOrElse(throw new AssertionError("error in uri parser")))

    // Json Types
    def `null`[_: P] = P("null").map(_ => Json.Null)
    def `false`[_: P] = P("false").map(_ => Json.False)
    def `true`[_: P] = P("true").map(_ => Json.True)
    def digits[_: P] = P(CharsWhileIn("0-9"))
    def exponent[_: P] = P(CharIn("eE") ~ CharIn("+\\-").? ~ digits)
    def fractional[_: P] = P("." ~ digits)
    def integral[_: P] = P("0" | CharIn("1-9") ~ digits.?)
    def number[_: P] = P(CharIn("+\\-").? ~ integral ~ fractional.? ~ exponent.?).!.map(x => Json.fromDouble(x.toDouble).get)
    def stringChars(c: Char) = c != '\"' && c != '\\'
    def space[_: P] = P(CharsWhileIn(" \r\n", 0))
    def strChars[_: P] = P(CharsWhile(stringChars))
    def hexDigit[_: P] = P(CharIn("0-9a-fA-F"))
    def unicodeEscape[_: P] = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
    def escape[_: P] = P("\\" ~ (CharIn("\"/\\\\bfnrt") | unicodeEscape))
    def string[_: P] = P(space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(Json.fromString)

    def nameChar[_: P] = P(CharIn("\\-_").! | digit | alpha)
    def attrname[_: P] = P((alpha ~ nameChar.rep).!)
    def subattr[_: P] = P("." ~ attrname)
    def attrPath[_: P] = P((uriPrefix ~ ":").? ~ attrname ~ subattr.?).map(v => AttributePath(v._2, v._1, v._3))
    def compareOp[_: P] = P(StringIn("eq", "ne", "co", "sw", "ew", "gt", "lt", "ge", "le")).!
      .map[(AttributePath, Json) => Comparison] {
        case "eq" => (p, v) => Comparison.Equal(p, v)
        case "ne" => (p, v) => Comparison.NotEqual(p, v)
        case "co" => (p, v) => Comparison.Contains(p, v)
        case "sw" => (p, v) => Comparison.StartsWith(p, v)
        case "ew" => (p, v) => Comparison.EndsWith(p, v)
        case "gt" => (p, v) => Comparison.GreaterThan(p, v)
        case "ge" => (p, v) => Comparison.GreaterThanOrEqual(p, v)
        case "lt" => (p, v) => Comparison.LessThan(p, v)
        case "le" => (p, v) => Comparison.LessThanOrEqual(p, v)
      }
    def compValue[_: P]: P[Json] = P(`false` | `null` | `true` | number | string)
    def logExp[_: P]: P[LogicalOperation] = P(filter ~ space ~ (P("and").log("and").map(_ => And) | P("or").log("or").map(_ => Or)) ~ space ~ filter).log
      .map { case (a, comb, b) => comb(a, b) }
    def attrExpPresent[_: P]: P[Comparison] = P(attrPath ~ space ~ "pr").map(attrPath => Comparison.Present(attrPath)).log
    def attrExpCompare[_: P]: P[Comparison] = (attrPath ~ space ~ compareOp ~ space ~ compValue).map { case (path, op, value) => op(path, value) }
    def attrExp[_: P]: P[Comparison] = P(attrExpPresent | attrExpCompare)
    def parensExp[_: P]: P[AFilter] = P(("not".!.? ~ space.? ~ "(" ~ valFilter ~ ")")).map {
      case (Some(_), filter) => Not(filter)
      case (None, filter) => filter
    }
    def valFilter[_: P]: P[AFilter] = P(logExp | parensExp | attrExp).log
    def valuePath[_: P]: P[ComplexAttributeFilter] = P(attrPath ~ "[" ~ valFilter ~ "]").map { case (path, filter) => ComplexAttributeFilter(path, filter) }
    def filter[_: P]: P[AFilter] = P(parensExp | attrExp | logExp | valuePath).log
    def completeFilter[_: P]: P[AFilter] = P(Start ~ filter ~ End)
    //    def completeFilter[_: P]: P[AFilter] = P(Start ~ logExp ~ End)
  }
}
