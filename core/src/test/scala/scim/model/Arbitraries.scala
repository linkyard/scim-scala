package scim.model

import java.net.URI
import io.circe.Json
import io.circe.testing.ArbitraryInstances
import org.scalacheck.{Arbitrary, Gen, ScalacheckShapeless}
import scim.model.Filter.Comparison._
import scim.model.Filter._
import scim.model.ResourceType.SchemaExtension

object Arbitraries {
  implicit val json: Arbitrary[Json] = new ArbitraryInstances {}.arbitraryJson

  implicit val uri: Arbitrary[URI] = Arbitrary(Gen.oneOf(
    URI.create("https://example.local/aaa"),
    URI.create("https://host.local/bbb"),
  ))

  implicit def value: Arbitrary[Value] = Arbitrary(Gen.frequency(
    (1, Gen.const(NullValue)),
    (2, implicitly[Arbitrary[Boolean]].arbitrary.map(BooleanValue)),
    (5, implicitly[Arbitrary[Double]].arbitrary.map(NumberValue)),
    (10, Gen.alphaNumStr.map(StringValue)),
  ))

  def schema: Arbitrary[Schema] = Arbitrary(Gen.frequency(
    10 -> Gen.const("urn:ietf:params:scim:schemas:core:2.0:User"),
    5 -> Gen.const("urn:ietf:params:scim:schemas:core:2.0:Group"),
    1 -> Gen.const("urn:custom:bla"),
  ).map(Schema.apply))
  def alpha: Gen[Char] = Gen.oneOf(('A' to 'Z') ++ ('a' to 'z'))
  def alphaNum: Gen[Char] = Gen.oneOf(('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9'))
  def attributeName: Arbitrary[String] = Arbitrary(for {
    first <- alpha
    more <- Gen.listOfN(10, alphaNum).map(_.mkString)
  } yield first.toString + more)
  implicit def attributePath: Arbitrary[AttributePath] = Arbitrary(for {
    name <- attributeName.arbitrary
    schema <- Gen.oneOf(Gen.const(None), schema.arbitrary.map(Some.apply))
    subPath <- Gen.oneOf(Gen.const(None), attributeName.arbitrary.map(Some.apply))
  } yield AttributePath(name, schema, subPath))
  implicit def filteredAttributePath: Arbitrary[FilteredAttributePath] = Arbitrary(for {
    name <- attributeName.arbitrary
    filter <- aValueFilter.arbitrary
    schema <- Gen.oneOf(Gen.const(None), schema.arbitrary.map(Some.apply))
    subPath <- Gen.oneOf(Gen.const(None), attributeName.arbitrary.map(Some.apply))
  } yield FilteredAttributePath(name, filter, schema, subPath))

  implicit def comparison: Arbitrary[Comparison] = Arbitrary(for {
    path <- attributePath.arbitrary
    value <- value.arbitrary
    op <- Gen.oneOf(Seq(
      Equal.apply _, NotEqual.apply _, Contains.apply _,
      StartsWith.apply _, EndsWith.apply _,
      GreaterThan.apply _, GreaterThanOrEqual.apply _,
      LessThan.apply _, LessThanOrEqual.apply _,
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

  implicit def patchOpOperation: Arbitrary[PatchOp.Operation] = Arbitrary(for {
    op <- Gen.oneOf(PatchOp.OperationType.Add, PatchOp.OperationType.Remove, PatchOp.OperationType.Replace)
    path <- Gen.frequency(
      1 -> Gen.const(None),
      10 -> attributePath.arbitrary.map(Some(_)),
      10 -> filteredAttributePath.arbitrary.map(Some(_)),
    )
    value <- json.arbitrary
  } yield (PatchOp.Operation(op, path, Some(value).filterNot(_.isNull))))

  implicit def patchOp: Arbitrary[PatchOp] = Arbitrary(Gen.nonEmptyListOf(patchOpOperation.arbitrary).map(ops => PatchOp(ops)))

  implicit def schemaExtension: Arbitrary[SchemaExtension] = Arbitrary(for {
    schema <- schema.arbitrary
    required <- Arbitrary.arbBool.arbitrary
  } yield SchemaExtension(schema, required))

  implicit def resourceType: Arbitrary[ResourceType] = Arbitrary(
    for {
      id <- attributeName.arbitrary
      externalId <- Gen.oneOf(None, Some(id))
      name <- attributeName.arbitrary
      description <- attributeName.arbitrary
      schemaExtensions <- Gen.listOf(schemaExtension.arbitrary)
    } yield ResourceType(
      id = id, externalId = externalId, name = name, description = description,
      endpoint = s"/$id", schemaExtensions = schemaExtensions
    ))
}
