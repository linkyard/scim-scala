package scim.model

import java.net.URI
import io.circe.Json
import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.{Arbitrary, Gen}
import scim.model.Filter.{AFilter, AttributePath, Comparison, LogicalOperation, NoFilter}

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

  implicit def filter: Arbitrary[Filter] = Arbitrary(Gen.oneOf(
    aFilter.arbitrary,
    Gen.const(NoFilter),
  ))

  implicit def aFilter: Arbitrary[AFilter] = Arbitrary(Gen.oneOf(
    implicitly[Arbitrary[LogicalOperation]].arbitrary,
    implicitly[Arbitrary[LogicalOperation]].arbitrary,
    implicitly[Arbitrary[Comparison]].arbitrary,
  ))

}
