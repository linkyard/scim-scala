package ch.linkyard.scim.model

import io.circe.Json
import io.circe.testing.ArbitraryInstances
import io.github.martinhh.derived.scalacheck.deriveArbitrary
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import ch.linkyard.scim.model.Filter.*
import ch.linkyard.scim.model.Filter.Comparison.*
import ch.linkyard.scim.model.ResourceType.SchemaExtension

import java.net.URI

object Arbitraries {
  given json: Arbitrary[Json] = new ArbitraryInstances {}.arbitraryJson

  implicit val uri: Arbitrary[URI] = Arbitrary(Gen.oneOf(
    URI.create("https://example.local/aaa"),
    URI.create("https://host.local/bbb"),
  ))

  given value: Arbitrary[Value] = Arbitrary(Gen.frequency(
    (1, Gen.const(NullValue)),
    (2, implicitly[Arbitrary[Boolean]].arbitrary.map(BooleanValue.apply)),
    (5, implicitly[Arbitrary[Double]].arbitrary.map(NumberValue.apply)),
    (10, Gen.alphaNumStr.map(StringValue.apply)),
  ))

  def schema: Arbitrary[Schema] = Arbitrary(Gen.frequency(
    10 -> Gen.const("urn:ietf:params:scim:schemas:core:2.0:User"),
    5 -> Gen.const("urn:ietf:params:scim:schemas:core:2.0:Group"),
    1 -> Gen.const("urn:custom:bla"),
  ).map(Schema.apply))
  def alpha: Gen[Char] = Gen.oneOf(('A' to 'Z') ++ ('a' to 'z'))
  def alphaNum: Gen[Char] = Gen.oneOf(('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9'))
  def attributeName: Arbitrary[String] = Arbitrary(for
    first <- alpha
    more <- Gen.listOfN(10, alphaNum).map(_.mkString)
  yield first.toString + more)
  given attributePath: Arbitrary[AttributePath] = Arbitrary(for
    name <- attributeName.arbitrary
    schema <- Gen.oneOf(Gen.const(None), schema.arbitrary.map(Some.apply))
    subPath <- Gen.oneOf(Gen.const(None), attributeName.arbitrary.map(Some.apply))
  yield AttributePath(name, schema, subPath))
  given filteredAttributePath: Arbitrary[FilteredAttributePath] = Arbitrary(for
    name <- attributeName.arbitrary
    filter <- aValueFilter.arbitrary
    schema <- Gen.oneOf(Gen.const(None), schema.arbitrary.map(Some.apply))
    subPath <- Gen.oneOf(Gen.const(None), attributeName.arbitrary.map(Some.apply))
  yield FilteredAttributePath(name, filter, schema, subPath))

  given comparison: Arbitrary[Comparison] = Arbitrary(for
    path <- attributePath.arbitrary
    value <- value.arbitrary
    op <- Gen.oneOf(Seq(
      Equal.apply,
      NotEqual.apply,
      Contains.apply,
      StartsWith.apply,
      EndsWith.apply,
      GreaterThan.apply,
      GreaterThanOrEqual.apply,
      LessThan.apply,
      LessThanOrEqual.apply,
    ))
  yield op(path, value))

  given logicalOperation: Arbitrary[LogicalOperation] = Arbitrary(for
    a <- aSimpleFilter.arbitrary
    b <- aSimpleFilter.arbitrary
    op <- Gen.oneOf(Seq(And.apply, Or.apply))
  yield op(a, b))

  given complexAttributeFilter: Arbitrary[ComplexAttributeFilter] = Arbitrary(for
    path <- attributePath.arbitrary
    aValueFilter <- aValueFilter.arbitrary
  yield ComplexAttributeFilter(path, aValueFilter))

  given aSimpleFilter: Arbitrary[AFilter] = {
    Arbitrary(Gen.frequency(
      (1, complexAttributeFilter.arbitrary),
      (2, comparison.arbitrary),
    ))
  }

  given aValueFilter: Arbitrary[AFilter] = {
    given log: Arbitrary[LogicalOperation] = Arbitrary(for
      a <- comparison.arbitrary
      b <- comparison.arbitrary
      op <- Gen.oneOf(Seq(And.apply, Or.apply))
    yield op(a, b))

    Arbitrary(Gen.frequency(
      (1, log.arbitrary),
      (3, comparison.arbitrary),
    ))
  }

  given aFilter: Arbitrary[AFilter] = {
    Arbitrary(Gen.frequency(
      (1, logicalOperation.arbitrary),
      (2, complexAttributeFilter.arbitrary),
      (4, aSimpleFilter.arbitrary),
    ))
  }

  given filter: Arbitrary[Filter] = Arbitrary(Gen.frequency(
    20 -> aFilter.arbitrary,
    1 -> Gen.const(NoFilter),
  ))

  given patchOpOperation: Arbitrary[PatchOp.Operation] = Arbitrary(for
    op <- Gen.oneOf(PatchOp.OperationType.Add, PatchOp.OperationType.Remove, PatchOp.OperationType.Replace)
    path <- Gen.frequency(
      1 -> Gen.const(None),
      10 -> attributePath.arbitrary.map(Some(_)),
      10 -> filteredAttributePath.arbitrary.map(Some(_)),
    )
    value <- json.arbitrary
  yield PatchOp.Operation(op, path, Some(value).filterNot(_.isNull)))

  given patchOp: Arbitrary[PatchOp] = Arbitrary(Gen.nonEmptyListOf(patchOpOperation.arbitrary).map(ops => PatchOp(ops)))

  given schemaExtension: Arbitrary[SchemaExtension] = Arbitrary(for
    schema <- schema.arbitrary
    required <- Arbitrary.arbBool.arbitrary
  yield SchemaExtension(schema, required))

  given resourceType: Arbitrary[ResourceType] = Arbitrary(
    for
      id <- attributeName.arbitrary
      name <- attributeName.arbitrary
      description <- attributeName.arbitrary
      schemaExtensions <- Gen.listOf(schemaExtension.arbitrary)
    yield ResourceType(
      id = id,
      name = name,
      description = description,
      endpoint = s"/$id",
      schemaExtensions = schemaExtensions,
    )
  )

  given Arbitrary[SortOrder] = deriveArbitrary

  given Arbitrary[ServiceProviderConfiguration] = deriveArbitrary
}
