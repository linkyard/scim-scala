package scim.model

import java.net.URI
import scala.util.Try
import cats.kernel.Monoid
import io.circe.optics.{JsonPath, JsonTraversalPath}
import io.circe.{ACursor, Decoder, HCursor, Json, JsonNumber}
import io.circe.optics.JsonPath._

/** RFC 7644 - 3.4.2.2. */
sealed trait Filter {
  def evaluate(on: Json, defaultSchema: Schema): Boolean

  /** use on root only, extracts the default schema */
  def evaluate(on: Json): Boolean = {
    val schema = on.hcursor.downField("schemas").as[Seq[String]].toOption.flatMap(_.headOption)
      .flatMap(a => Schema.parse(a).toOption)
      .getOrElse(Schema.default)
    evaluate(on, schema)
  }

  def render: String
  def asString: String = render
}

object Filter {
  def parse(string: String): Either[String, Filter] = Parser.parseFilter(string)

  def parseAttributeSelector(string: String): Either[String, AttributeSelector] = Parser.parseAttributeSelector(string)

  def none: Filter = NoFilter

  sealed trait AFilter extends Filter
  case object NoFilter extends Filter {
    def evaluate(on: Json, defaultSchema: Schema): Boolean = true
    def render = ""
  }

  sealed trait LogicalOperation extends AFilter
  case class And(a: AFilter, b: AFilter) extends LogicalOperation {
    def evaluate(on: Json, defaultSchema: Schema): Boolean =
      a.evaluate(on, defaultSchema) && b.evaluate(on, defaultSchema)
    def render = s"${a.render} and ${b.render}"
  }
  case class Or(a: AFilter, b: AFilter) extends LogicalOperation {
    def evaluate(on: Json, defaultSchema: Schema): Boolean =
      a.evaluate(on, defaultSchema) || b.evaluate(on, defaultSchema)
    def render = s"${a.render} or ${b.render}"
  }
  case class Not(a: AFilter) extends LogicalOperation {
    def evaluate(on: Json, defaultSchema: Schema): Boolean = !a.evaluate(on, defaultSchema)
    def render = s"not (${a.render})"
  }

  sealed trait Comparison extends AFilter {
    def attributePath: AttributePath
    def attributeValue(on: Json, defaultSchema: Schema): Json = attributePath.extract(on, defaultSchema)
  }
  object Comparison {
    case class Equal(attributePath: AttributePath, value: Value) extends Comparison {
      def evaluate(on: Json, defaultSchema: Schema): Boolean = value.isEqualTo(attributeValue(on, defaultSchema))
      def render = s"${attributePath.render} eq ${value.render}"
    }
    case class NotEqual(attributePath: AttributePath, value: Value) extends Comparison {
      def evaluate(on: Json, defaultSchema: Schema): Boolean = !value.isEqualTo(attributeValue(on, defaultSchema))
      def render = s"${attributePath.render} ne ${value.render}"
    }
    case class Contains(attributePath: AttributePath, value: Value) extends Comparison {
      def evaluate(on: Json, defaultSchema: Schema): Boolean = value.isContainedIn(attributeValue(on, defaultSchema))
      def render = s"${attributePath.render} co ${value.render}"
    }
    case class StartsWith(attributePath: AttributePath, value: Value) extends Comparison {
      def evaluate(on: Json, defaultSchema: Schema): Boolean = value.isPrefixOf(attributeValue(on, defaultSchema))
      def render = s"${attributePath.render} sw ${value.render}"
    }
    case class EndsWith(attributePath: AttributePath, value: Value) extends Comparison {
      def evaluate(on: Json, defaultSchema: Schema): Boolean = value.isSuffixOf(attributeValue(on, defaultSchema))
      def render = s"${attributePath.render} ew ${value.render}"
    }
    case class Present(attributePath: AttributePath) extends Comparison {
      def evaluate(on: Json, defaultSchema: Schema): Boolean = !attributeValue(on, defaultSchema).isNull
      def render = s"${attributePath.render} pr"
    }
    case class GreaterThan(attributePath: AttributePath, value: Value) extends Comparison {
      def evaluate(on: Json, defaultSchema: Schema): Boolean = value.isLessThan(attributeValue(on, defaultSchema))
      def render = s"${attributePath.render} gt ${value.render}"
    }
    case class GreaterThanOrEqual(attributePath: AttributePath, value: Value) extends Comparison {
      def evaluate(on: Json, defaultSchema: Schema): Boolean = {
        val v = attributeValue(on, defaultSchema)
        value.isLessThan(v) || value.isEqualTo(v)
      }
      def render = s"${attributePath.render} ge ${value.render}"
    }
    case class LessThan(attributePath: AttributePath, value: Value) extends Comparison {
      def evaluate(on: Json, defaultSchema: Schema): Boolean = value.isGreaterThan(attributeValue(on, defaultSchema))
      def render = s"${attributePath.render} lt ${value.render}"
    }
    case class LessThanOrEqual(attributePath: AttributePath, value: Value) extends Comparison {
      def evaluate(on: Json, defaultSchema: Schema): Boolean = {
        val v = attributeValue(on, defaultSchema)
        value.isGreaterThan(v) || value.isEqualTo(v)
      }
      def render = s"${attributePath.render} le ${value.render}"
    }
  }

  case class ComplexAttributeFilter(attributePath: AttributePath, filter: AFilter) extends AFilter {
    def evaluate(on: Json, defaultSchema: Schema): Boolean = {
      val value = attributePath.extract(on, defaultSchema)
      value.asArray.getOrElse(Vector(value))
        .exists(v => filter.evaluate(v, defaultSchema))
    }
    def render = s"${attributePath.render}[${filter.render}]"
  }

  sealed trait AttributeSelector {
    def render: String

    def jsonPath(defaultSchema: Schema) =
      jsonPathSingle(defaultSchema).json.asTraversal.choice(jsonPathMulti(defaultSchema).json)

    def jsonPathSingle(defaultSchema: Schema): JsonPath
    def jsonPathMulti(defaultSchema: Schema): JsonTraversalPath
  }

  case class AttributePath(name: String, schema: Option[Schema] = None, subAttribute: Option[String] = None) extends AttributeSelector {
    private def attrBase(defaultSchema: Schema): JsonPath =
      schema.filter(_ != defaultSchema).map(_.asString).map(root.selectDynamic).getOrElse(root).selectDynamic(name)

    def jsonPathSingle(defaultSchema: Schema): JsonPath =
      subAttribute.map(attrBase(defaultSchema).selectDynamic).getOrElse(attrBase(defaultSchema))
    def jsonPathMulti(defaultSchema: Schema): JsonTraversalPath =
      subAttribute.map(attrBase(defaultSchema).each.selectDynamic).getOrElse(attrBase(defaultSchema).each)

    def extract(from: Json, defaultSchema: Schema): Json = {
      val values = jsonPathSingle(defaultSchema).json.getOption(from)
        .orElse(Some(jsonPathMulti(defaultSchema).json.getAll(from)).filter(_.nonEmpty).map(l => Json.arr(l: _*)))
        .getOrElse(Json.Null)
      println(values)
      values
    }
    def render: String = schema.map(_.asString + ":").getOrElse("") + name + subAttribute.map("." + _).getOrElse("")
  }
  case class FilteredAttributePath(name: String, filter: AFilter, schema: Option[Schema] = None, subAttribute: Option[String] = None)
    extends AttributeSelector {
    private def attrBase(defaultSchema: Schema): JsonPath =
      schema.filter(_ != defaultSchema).map(_.asString).map(root.selectDynamic).getOrElse(root).selectDynamic(name)

    def jsonPathSingle(defaultSchema: Schema): JsonPath =
      subAttribute.map(attrBase(defaultSchema).selectDynamic).getOrElse(attrBase(defaultSchema))

    def jsonPathMulti(defaultSchema: Schema): JsonTraversalPath =
      subAttribute.map(attrBase(defaultSchema).each.selectDynamic).getOrElse(attrBase(defaultSchema).each)

    def render: String = schema.map(_.asString + ":").getOrElse("") + name + s"[${filter.render}]" + subAttribute.map("." + _).getOrElse("")
  }

  sealed trait Value {
    def isEqualTo(other: Json): Boolean
    def isContainedIn(other: Json): Boolean
    def isPrefixOf(other: Json): Boolean
    def isSuffixOf(other: Json): Boolean
    def isGreaterThan(other: Json): Boolean
    def isLessThan(other: Json): Boolean
    def render: String

    protected def valueOf[X: Decoder](from: Json): Iterable[X] = {
      from.as[Seq[X]].toOption
        .orElse(from.as[X].toOption.map(Seq(_)))
        .getOrElse(Seq.empty)
    }
  }
  case class StringValue(value: String) extends Value {
    def isEqualTo(other: Json): Boolean = stringValue(other).exists(_ == value)
    def isContainedIn(other: Json): Boolean = stringValue(other).exists(_.contains(value))
    def isPrefixOf(other: Json): Boolean = stringValue(other).exists(_.startsWith(value))
    def isSuffixOf(other: Json): Boolean = stringValue(other).exists(_.endsWith(value))
    def isGreaterThan(other: Json): Boolean = stringValue(other).exists(_.compareTo(value) < 0)
    def isLessThan(other: Json): Boolean = stringValue(other).exists(_.compareTo(value) > 0)
    private def stringValue(from: Json) = valueOf[String](from)

    def render: String = Json.fromString(value).noSpaces
  }
  case class NumberValue(value: Double) extends Value {
    def isEqualTo(other: Json): Boolean = valueOf[Json](other).exists(_ == Json.fromDoubleOrNull(value))
    def isContainedIn(other: Json): Boolean = false
    def isPrefixOf(other: Json): Boolean = false
    def isSuffixOf(other: Json): Boolean = false
    def isGreaterThan(other: Json): Boolean = doubleValue(other).exists(_.compareTo(value) < 0)
    def isLessThan(other: Json): Boolean = doubleValue(other).exists(_.compareTo(value) > 0)
    private def doubleValue(from: Json) = valueOf[Double](from)

    def render: String = Json.fromDoubleOrString(value).noSpaces
  }
  case class BooleanValue(value: Boolean) extends Value {
    def isEqualTo(other: Json): Boolean = valueOf[Boolean](other).exists(_ == value)
    def isContainedIn(other: Json): Boolean = false
    def isPrefixOf(other: Json): Boolean = false
    def isSuffixOf(other: Json): Boolean = false
    def isGreaterThan(other: Json): Boolean = false
    def isLessThan(other: Json): Boolean = false
    def render: String = if (value) "true" else "false"
  }
  case object NullValue extends Value {
    def isEqualTo(other: Json): Boolean = other.isNull
    def isContainedIn(other: Json): Boolean = false
    def isPrefixOf(other: Json): Boolean = false
    def isSuffixOf(other: Json): Boolean = false
    def isGreaterThan(other: Json): Boolean = false
    def isLessThan(other: Json): Boolean = false
    def render = "null"
  }

  private object Parser {
    import fastparse._
    import NoWhitespace._

    def parseFilter(string: String): Either[String, Filter] = {
      if (string.trim.isEmpty) Right(NoFilter)
      else {
        fastparse.parse(string, completeFilter(_), verboseFailures = true) match {
          case Parsed.Success(value, index) => Right(value)
          case failure: Parsed.Failure => Left(failure.longMsg)
        }
      }
    }

    def parseAttributeSelector(string: String): Either[String, AttributeSelector] = {
      fastparse.parse(string, attributeSelector(_), verboseFailures = true) match {
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
    def `null`[_: P] = P("null").map(_ => NullValue)
    def `false`[_: P] = P("false").map(_ => BooleanValue(false))
    def `true`[_: P] = P("true").map(_ => BooleanValue(true))
    def digits[_: P] = P(CharsWhileIn("0-9"))
    def exponent[_: P] = P(CharIn("eE") ~ CharIn("+\\-").? ~ digits)
    def fractional[_: P] = P("." ~ digits)
    def integral[_: P] = P("0" | CharIn("1-9") ~ digits.?)
    def number[_: P] = P(CharIn("+\\-").? ~ integral ~ fractional.? ~ exponent.?).!.map(_.toDouble).map(NumberValue)
    def stringChars(c: Char) = c != '\"' && c != '\\'
    def space[_: P] = P(CharsWhileIn(" \r\n", 0))
    def strChars[_: P] = P(CharsWhile(stringChars))
    def hexDigit[_: P] = P(CharIn("0-9a-fA-F"))
    def unicodeEscape[_: P] = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
    def escape[_: P] = P("\\" ~ (CharIn("\"/\\\\bfnrt") | unicodeEscape))
    def string[_: P] = P(space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(StringValue)

    def nameChar[_: P] = P(CharIn("\\-_").! | digit | alpha)
    def attrname[_: P] = P((alpha ~ nameChar.rep).!)
    def subattr[_: P] = P("." ~ attrname)
    def attrPath[_: P] = P((uriPrefix ~ ":").? ~ attrname ~ subattr.?).map(v => AttributePath(v._2, v._1.map(Schema.apply), v._3))
    def filteredAttrPath[_: P]: P[FilteredAttributePath] = P(attrPath ~ "[" ~/ valFilter ~ "]" ~ subattr.?).map {
      case (path, filter, sub) => FilteredAttributePath(path.name, filter, path.schema, sub)
    }
    def compareOp[_: P] = P(StringInIgnoreCase("eq", "ne", "co", "sw", "ew", "gt", "lt", "ge", "le")).!
      .map(_.toLowerCase)
      .map[(AttributePath, Value) => Comparison] {
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
    def compValue[_: P]: P[Value] = P(`false` | `null` | `true` | number | string)


    def attrExpPresent[_: P]: P[Comparison] = P(attrPath ~ space ~ IgnoreCase("pr")).map(attrPath => Comparison.Present(attrPath))
    def attrExpCompare[_: P]: P[Comparison] = (attrPath ~ space ~ compareOp ~ space ~ compValue).map { case (path, op, value) => op(path, value) }
    def attrExp[_: P]: P[Comparison] = P(attrExpPresent | attrExpCompare)
    def parensExp[_: P]: P[AFilter] = P(((IgnoreCase("not") ~ space).!.? ~ "(" ~/ filter ~ ")")).map {
      case (Some(_), filter) => Not(filter)
      case (None, filter) => filter
    }
    def valuePath[_: P]: P[ComplexAttributeFilter] = P(attrPath ~ "[" ~/ valFilter ~ "]").map { case (path, filter) => ComplexAttributeFilter(path, filter) }
    def logicalOperator[_: P]: P[(AFilter, AFilter) => LogicalOperation] = (P(IgnoreCase("and")).map(_ => And) | P(IgnoreCase("or")).map(_ => Or))

    def valFilter[_: P]: P[AFilter] = P((parensExp | attrExp) ~ (space ~ logicalOperator ~ space ~ valFilter).?).map {
      case (a, None) => a
      case (a, Some((op, b))) => op(a, b)
    }
    def filter[_: P]: P[AFilter] = P((parensExp | valuePath | attrExp) ~ (space ~ logicalOperator ~ space ~ filter).?).map {
      case (a, None) => a
      case (a, Some((op, b))) => op(a, b)
    }

    def completeFilter[_: P]: P[AFilter] = P(Start ~ filter ~ End)

    def attributeSelector[_: P]: P[AttributeSelector] = P(Start ~ (filteredAttrPath | attrPath) ~ End)
  }
}
