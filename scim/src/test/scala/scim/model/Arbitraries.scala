package scim.model

import java.net.URI
import io.circe.Json
import org.scalacheck.{Arbitrary, Gen}
import scim.model.Filter.Comparison.{Contains, EndsWith, Equal, GreaterThan, GreaterThanOrEqual, LessThan, LessThanOrEqual, NotEqual, Present, StartsWith}
import scim.model.Filter.{AFilter, And, AttributePath, Comparison, ComplexAttributeFilter, LogicalOperation, NoFilter, Or}

object Arbitraries {
  implicit def json: Arbitrary[Json] = Arbitrary(Gen.frequency(
    (1, Gen.const(Json.Null)),
    (3, implicitly[Arbitrary[Double]].arbitrary.map(Json.fromDoubleOrString)),
    (10, Gen.alphaNumStr.map(Json.fromString)),
  ))

  def schemaUri: Arbitrary[URI] = Arbitrary(Gen.frequency(
    10 -> Gen.const(URI.create("urn:ietf:params:scim:schemas:core:2.0:User")),
    5 -> Gen.const(URI.create("urn:ietf:params:scim:schemas:core:2.0:Group")),
    1 -> Gen.const(URI.create("urn:custom:bla")),
  ))
  def alpha: Gen[Char] = Gen.oneOf(('A' to 'Z') ++ ('a' to 'z'))
  def alphaNum: Gen[Char] = Gen.oneOf(('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9'))
  def attributeName: Arbitrary[String] = Arbitrary(for {
    first <- alpha
    more <- Gen.listOfN(10, alphaNum).map(_.mkString)
  } yield first.toString + more)
  implicit def attributePath: Arbitrary[AttributePath] = Arbitrary(for {
    name <- attributeName.arbitrary
    uri <- Gen.oneOf(Gen.const(None), schemaUri.arbitrary.map(Some.apply))
    subPath <- Gen.oneOf(Gen.const(None), attributeName.arbitrary.map(Some.apply))
  } yield AttributePath(name, uri, subPath))

  implicit def comparison: Arbitrary[Comparison] = Arbitrary(for {
    path <- attributePath.arbitrary
    value <- json.arbitrary
    op <- Gen.oneOf(Seq(
      Equal.apply _, NotEqual.apply _, Contains.apply _,
      StartsWith.apply _, EndsWith.apply _,
      GreaterThan.apply _, GreaterThanOrEqual.apply _,
      LessThan.apply _, LessThanOrEqual.apply _,
      //      (p, _: Json) => Present(p)
    ))
  } yield op(path, value))

  implicit def logicalOperation: Arbitrary[LogicalOperation] = Arbitrary(for {
    a <- aSimpleFilter.arbitrary
    b <- aSimpleFilter.arbitrary
    op <- Gen.oneOf(Seq(And.apply _, Or.apply _))
  } yield And(a, b))

  implicit def complexAttributeFilter: Arbitrary[ComplexAttributeFilter] = Arbitrary(for {
    path <- attributePath.arbitrary
    aValueFilter <- aValueFilter.arbitrary
  } yield ComplexAttributeFilter(path, aValueFilter))

  implicit def aSimpleFilter: Arbitrary[AFilter] = {
    Arbitrary(Gen.frequency(
      (1, complexAttributeFilter.arbitrary),
      (2, comparison.arbitrary),
    ))
  }

  implicit def aValueFilter: Arbitrary[AFilter] = {
    implicit def log: Arbitrary[LogicalOperation] = Arbitrary(for {
      a <- comparison.arbitrary
      b <- comparison.arbitrary
      op <- Gen.oneOf(Seq(And.apply _, Or.apply _))
    } yield And(a, b))

    Arbitrary(Gen.frequency(
      (1, log.arbitrary),
      (3, comparison.arbitrary),
    ))
  }

  implicit def aFilter: Arbitrary[AFilter] = {
    Arbitrary(Gen.frequency(
      (1, logicalOperation.arbitrary),
      (2, complexAttributeFilter.arbitrary),
      (4, aSimpleFilter.arbitrary),
    ))
  }

  implicit def filter: Arbitrary[Filter] = Arbitrary(Gen.frequency(
    20 -> aFilter.arbitrary,
    1 -> Gen.const(NoFilter),
  ))
}
