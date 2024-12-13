package scim.model

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers
import scim.model.Arbitraries.given
import scim.model.Filter.*
import scim.model.Filter.Comparison.Equal

import scala.language.implicitConversions

class FilterSpec extends AnyFunSpec with Checkers with Matchers {

  private def parseFilter(value: String): Filter = {
    Filter.parse(value) match {
      case Right(result) => result
      case Left(error)   => fail(s"parsing of '$value' failed: $error")
    }
  }
  private def parseAS(value: String): AttributeSelector = {
    Filter.parseAttributeSelector(value) match {
      case Right(result) => result
      case Left(error)   => fail(s"parsing of '$value' failed: $error")
    }
  }

  describe("Filter") {
    describe("parseFilter") {
      it("and asString should be compatible")(
        check { (filter: Filter) =>
          val string = filter.asString
          Filter.parse(string) == Right(filter)
        },
        minSize(2000),
      )

      it("should be default for empty") {
        Filter.parse("") should be(Right(Filter.none))
        Filter.parse(" ") should be(Right(Filter.none))
      }

      it("should be left from invalid") {
        Filter.parse("bla").isLeft should be(true)
        Filter.parse("hube").isLeft should be(true)
      }

      implicit def stringToValue(s: String): Value = StringValue(s)

      it("should parse simple eq filter") {
        val r = parseFilter("userName eq \"bjensen\"")
        r should be(Comparison.Equal(AttributePath("userName"), "bjensen"))
      }
      it("should parse simple eq filter in parens") {
        val r = parseFilter("(userName eq \"bjensen\")")
        r should be(Comparison.Equal(AttributePath("userName"), "bjensen"))
      }
      it("should parse subpath co filter") {
        val r = parseFilter("name.familyName co \"O'Malley\"")
        r should be(Comparison.Contains(AttributePath("name", subAttribute = Some("familyName")), "O'Malley"))
      }
      it("should parse simple sw filter") {
        val r = parseFilter("userName sw \"J\"")
        r should be(Comparison.StartsWith(AttributePath("userName"), "J"))
      }
      it("should parse sw filter with uri") {
        val r = parseFilter("urn:ietf:params:scim:schemas:core:2.0:User:userName sw \"J\"")
        r should be(Comparison.StartsWith(
          AttributePath("userName", schema = Some(Schema("urn:ietf:params:scim:schemas:core:2.0:User"))),
          "J",
        ))
      }
      it("should parse simple pr filter") {
        val r = parseFilter("title pr")
        r should be(Comparison.Present(AttributePath("title")))
      }
      it("should parse simple pr filter in parens") {
        val r = parseFilter("title pr")
        r should be(Comparison.Present(AttributePath("title")))
      }
      it("should parse a gt filter") {
        val r = parseFilter("meta.lastModified gt \"2011-05-13T04:42:34Z\"")
        r should be(Comparison.GreaterThan(
          AttributePath("meta", subAttribute = Some("lastModified")),
          "2011-05-13T04:42:34Z",
        ))
      }
      it("should parse a ge filter") {
        val r = parseFilter("meta.lastModified ge \"2011-05-13T04:42:34Z\"")
        r should be(Comparison.GreaterThanOrEqual(
          AttributePath("meta", subAttribute = Some("lastModified")),
          "2011-05-13T04:42:34Z",
        ))
      }
      it("should parse a lt filter") {
        val r = parseFilter("meta.lastModified lt \"2011-05-13T04:42:34Z\"")
        r should be(Comparison.LessThan(
          AttributePath("meta", subAttribute = Some("lastModified")),
          "2011-05-13T04:42:34Z",
        ))
      }
      it("should parse a le filter") {
        val r = parseFilter("meta.lastModified le \"2011-05-13T04:42:34Z\"")
        r should be(Comparison.LessThanOrEqual(
          AttributePath("meta", subAttribute = Some("lastModified")),
          "2011-05-13T04:42:34Z",
        ))
      }
      it("should parse an 'and' combination") {
        val r = parseFilter("title pr and userType eq \"Employee\"")
        r should be(
          And(
            Comparison.Present(AttributePath("title")),
            Comparison.Equal(AttributePath("userType"), "Employee"),
          )
        )
      }
      it("should parse an 'and' combination in parens") {
        val r = parseFilter("(title pr) and (userType eq \"Employee\")")
        r should be(
          And(
            Comparison.Present(AttributePath("title")),
            Comparison.Equal(AttributePath("userType"), "Employee"),
          )
        )
      }
      it("should parse an 'or' combination") {
        val r = parseFilter("title pr or userType eq \"Intern\"")
        r should be(
          Or(
            Comparison.Present(AttributePath("title")),
            Comparison.Equal(AttributePath("userType"), "Intern"),
          )
        )
      }
      it("should parse an 'or' combination in parens") {
        val r = parseFilter("(title pr) or (userType eq \"Intern\")")
        r should be(
          Or(
            Comparison.Present(AttributePath("title")),
            Comparison.Equal(AttributePath("userType"), "Intern"),
          )
        )
      }
      it("should parse another eq filter") {
        val r = parseFilter("schemas eq \"urn:ietf:params:scim:schemas:extension:enterprise:2.0:User\"")
        r should be(Comparison.Equal(
          AttributePath("schemas"),
          "urn:ietf:params:scim:schemas:extension:enterprise:2.0:User",
        ))
      }

      it("should parse an combination of and with or and parens") {
        val r =
          parseFilter("userType eq \"Employee\" and (emails co \"example.com\" or emails.value co \"example.org\")")
        r should be(
          And(
            Comparison.Equal(AttributePath("userType"), "Employee"),
            Or(
              Comparison.Contains(AttributePath("emails"), "example.com"),
              Comparison.Contains(AttributePath("emails", subAttribute = Some("value")), "example.org"),
            ),
          )
        )
      }

      it("should parse an combination of and with not, or and parens") {
        val r =
          parseFilter("userType ne \"Employee\" and not (emails co \"example.com\" or emails.value co \"example.org\")")
        r should be(
          And(
            Comparison.NotEqual(AttributePath("userType"), "Employee"),
            Not(
              Or(
                Comparison.Contains(AttributePath("emails"), "example.com"),
                Comparison.Contains(AttributePath("emails", subAttribute = Some("value")), "example.org"),
              )
            ),
          )
        )
      }

      it("should parse an combination of and with parens") {
        val r = parseFilter("userType eq \"Employee\" and (emails.type eq \"work\")")
        r should be(
          And(
            Comparison.Equal(AttributePath("userType"), "Employee"),
            Comparison.Equal(AttributePath("emails", subAttribute = Some("type")), "work"),
          )
        )
      }

      it("should parse an combination of and with value filter") {
        val r = parseFilter("userType eq \"Employee\" and emails[type eq \"work\" and value co \"@example.com\"]")
        r should be(
          And(
            Comparison.Equal(AttributePath("userType"), "Employee"),
            ComplexAttributeFilter(
              AttributePath("emails"),
              And(
                Comparison.Equal(AttributePath("type"), "work"),
                Comparison.Contains(AttributePath("value"), "@example.com"),
              ),
            ),
          )
        )
      }

      it("should parse an combination of or with two complex value filters") {
        val r = parseFilter(
          "emails[type eq \"work\" and value co \"@example.com\"] or ims[type eq \"xmpp\" and value co \"@foo.com\"]"
        )
        r should be(
          Or(
            ComplexAttributeFilter(
              AttributePath("emails"),
              And(
                Comparison.Equal(AttributePath("type"), "work"),
                Comparison.Contains(AttributePath("value"), "@example.com"),
              ),
            ),
            ComplexAttributeFilter(
              AttributePath("ims"),
              And(
                Comparison.Equal(AttributePath("type"), "xmpp"),
                Comparison.Contains(AttributePath("value"), "@foo.com"),
              ),
            ),
          )
        )
      }
    }

    describe("evaluate") {
      it("should evaluate simple equal (positive)") {
        val filter = parseFilter("userName eq \"bjensen@example.com\"")
        filter.evaluate(Jsons.userMinimal) should be(true)
      }
      it("should evaluate simple equal (negative)") {
        val filter = parseFilter("userName eq \"mario@example.com\"")
        filter.evaluate(Jsons.userMinimal) should be(false)
      }

      it("should evaluate negated expression") {
        val filter = parseFilter("not (userName eq \"mario@example.com\")")
        filter.evaluate(Jsons.userMinimal) should be(true)
      }

      it("should evaluate subpath contains (positive)") {
        parseFilter("name.familyName co \"Jen\"")
          .evaluate(Jsons.userFull) should be(true)
        parseFilter("name.familyName co \"Jensen\"")
          .evaluate(Jsons.userFull) should be(true)
        parseFilter("name.familyName co \"en\"")
          .evaluate(Jsons.userFull) should be(true)
      }
      it("should evaluate subpath contains (negative)") {
        parseFilter("name.familyName co \"X\"")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("name.familyName co \"bla\"")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("name.familyName co \"jen\"")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("name.familyName co 1")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("name.familyName co false")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("name.familyName co null")
          .evaluate(Jsons.userFull) should be(false)
      }

      it("should evaluate subpath startsWith (positive)") {
        parseFilter("name.familyName sw \"Jen\"")
          .evaluate(Jsons.userFull) should be(true)
        parseFilter("name.familyName sw \"Jensen\"")
          .evaluate(Jsons.userFull) should be(true)
        parseFilter("name.familyName sw \"J\"")
          .evaluate(Jsons.userFull) should be(true)
      }
      it("should evaluate subpath startsWith (negative)") {
        parseFilter("name.familyName sw \"X\"")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("name.familyName sw \"jen\"")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("name.familyName sw \"ens\"")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("name.familyName sw 1")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("name.familyName sw false")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("name.familyName sw null")
          .evaluate(Jsons.userFull) should be(false)
      }
      it("should evaluate subpath endsWith (positive)") {
        parseFilter("name.familyName ew \"en\"")
          .evaluate(Jsons.userFull) should be(true)
        parseFilter("name.familyName ew \"Jensen\"")
          .evaluate(Jsons.userFull) should be(true)
        parseFilter("name.familyName ew \"n\"")
          .evaluate(Jsons.userFull) should be(true)
      }
      it("should evaluate subpath endsWith (negative)") {
        parseFilter("name.familyName ew \"X\"")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("name.familyName ew \"eN\"")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("name.familyName ew \"N\"")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("name.familyName ew 1")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("name.familyName ew false")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("name.familyName ew null")
          .evaluate(Jsons.userFull) should be(false)
      }

      it("should evaluate presence (positive)") {
        parseFilter("name pr")
          .evaluate(Jsons.userFull) should be(true)
        parseFilter("userName pr")
          .evaluate(Jsons.userFull) should be(true)
        parseFilter("title pr")
          .evaluate(Jsons.userFull) should be(true)
      }
      it("should evaluate presence (negative)") {
        parseFilter("bla pr")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("urn:a:b:userName pr")
          .evaluate(Jsons.userFull) should be(false)
      }

      it("should evaluate greaterThan (positive)") {
        parseFilter("meta.lastModified gt \"2011-05-13T04:42:33Z\"")
          .evaluate(Jsons.userFull) should be(true)
        parseFilter("meta.lastModified gt \"2010-05-13T04:42:34Z\"")
          .evaluate(Jsons.userFull) should be(true)
      }
      it("should evaluate greaterThan (negative)") {
        parseFilter("meta.lastModified gt \"2013-05-13T04:42:34Z\"")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("meta.lastModified gt \"2011-05-13T04:42:34Z\"")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("name.familyName gt false")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("name.familyName gt null")
          .evaluate(Jsons.userFull) should be(false)
      }
      it("should evaluate greaterThanOrEqual (positive)") {
        parseFilter("meta.lastModified ge \"2011-05-13T04:42:34Z\"")
          .evaluate(Jsons.userFull) should be(true)
        parseFilter("meta.lastModified ge \"2011-05-13T04:42:33Z\"")
          .evaluate(Jsons.userFull) should be(true)
        parseFilter("meta.lastModified ge \"2010-05-13T04:42:34Z\"")
          .evaluate(Jsons.userFull) should be(true)
      }
      it("should evaluate greaterThanOrEqual (negative)") {
        parseFilter("meta.lastModified ge \"2013-05-13T04:42:34Z\"")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("name.familyName ge false")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("name.familyName ge null")
          .evaluate(Jsons.userFull) should be(false)
      }

      it("should evaluate lessThan (positive)") {
        parseFilter("meta.lastModified lt \"2011-05-13T04:42:35\"")
          .evaluate(Jsons.userFull) should be(true)
        parseFilter("meta.lastModified lt \"2013-05-13T04:42:34Z\"")
          .evaluate(Jsons.userFull) should be(true)
      }
      it("should evaluate lessThan (negative)") {
        parseFilter("meta.lastModified lt \"2010-05-13T04:42:34Z\"")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("meta.lastModified lt \"2011-05-13T04:42:34Z\"")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("name.familyName lt false")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("name.familyName lt null")
          .evaluate(Jsons.userFull) should be(false)
      }
      it("should evaluate lessThanOrEqual (positive)") {
        parseFilter("meta.lastModified le \"2011-05-13T04:42:34Z\"")
          .evaluate(Jsons.userFull) should be(true)
        parseFilter("meta.lastModified le \"2011-05-13T04:42:35Z\"")
          .evaluate(Jsons.userFull) should be(true)
        parseFilter("meta.lastModified le \"2013-05-13T04:42:34Z\"")
          .evaluate(Jsons.userFull) should be(true)
      }
      it("should evaluate lessThanOrEqual (negative)") {
        parseFilter("meta.lastModified le \"2010-05-13T04:42:34Z\"")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("name.familyName lt false")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("name.familyName lt null")
          .evaluate(Jsons.userFull) should be(false)
      }

      it("should evaluate equal with namespace (positive)") {
        val filter = parseFilter("urn:ietf:params:scim:schemas:core:2.0:User:userName eq \"bjensen@example.com\"")
        filter.evaluate(Jsons.userMinimal) should be(true)
      }
      it("should evaluate equal with namespace (negative)") {
        parseFilter("urn:ietf:params:scim:schemas:core:2.0:User:userName eq \"mario@example.com\"")
          .evaluate(Jsons.userMinimal) should be(false)
        parseFilter("urn:ietf:params:scim:schemas:core:2.0:Group:userName eq \"bjensen@example.com\"")
          .evaluate(Jsons.userMinimal) should be(false)
      }

      it("should evaluate 'and' combination (positive)") {
        val filter = parseFilter("title pr and userType eq \"Employee\"")
        filter.evaluate(Jsons.userFull) should be(true)
      }
      it("should evaluate 'and' combination (negative)") {
        parseFilter("not (title pr) and userType eq \"Employee\"")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("title pr and userType ne \"Employee\"")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("title pr and userType eq \"Homer\"")
          .evaluate(Jsons.userFull) should be(false)
      }

      it("should evaluate 'or' combination (positive)") {
        parseFilter("title pr or userType eq \"Employee\"")
          .evaluate(Jsons.userFull) should be(true)
        parseFilter("bla pr or userType eq \"Employee\"")
          .evaluate(Jsons.userFull) should be(true)
        parseFilter("title pr or userType eq \"Manager\"")
          .evaluate(Jsons.userFull) should be(true)
      }
      it("should evaluate 'or' combination (negative)") {
        parseFilter("not (title pr) or not (userType eq \"Employee\")")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("bla pr and userType ne \"Employee\"")
          .evaluate(Jsons.userFull) should be(false)
      }
      it("should evaluate an and/or combination with parens (positive)") {
        parseFilter("userType eq \"Employee\" and (emails.value co \"example.com\" or emails.value co \"example.org\")")
          .evaluate(Jsons.userFull) should be(true)
        parseFilter("userType eq \"Employee\" and (emails.value co \"acme.com\" or emails.value co \"example.com\")")
          .evaluate(Jsons.userFull) should be(true)
        parseFilter("userType eq \"Employee\" and (emails.value co \"example.com\" or emails.value co \"example.org\")")
          .evaluate(Jsons.userFull) should be(true)
      }
      it("should evaluate an and/or combination with parens (negative)") {
        parseFilter("userType eq \"Manager\" and (emails.value co \"example.com\" or emails.value co \"example.org\")")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("userType eq \"Employee\" and (emails.value co \"acme.com\" or emails.value co \"acme.org\")")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("userType eq \"Employee\" and (emails.value co \"acme.com\" or emails.value co \"example.x\")")
          .evaluate(Jsons.userFull) should be(false)
      }

      it("should evaluate eq on single element array (positive)") {
        parseFilter("schemas eq \"urn:ietf:params:scim:schemas:core:2.0:User\"")
          .evaluate(Jsons.userMinimal) should be(true)
        parseFilter("schemas eq \"urn:ietf:params:scim:schemas:extension:enterprise:2.0:User\"")
          .evaluate(Jsons.userFull) should be(true)
      }
      it("should evaluate eq on single element array (negative)") {
        parseFilter("schemas eq \"urn:ietf:params:scim:schemas:core:2.0:Group\"")
          .evaluate(Jsons.userMinimal) should be(false)
        parseFilter("schemas eq \"urn:ietf:params:scim:schemas:extension:enterprise:2.0:User\"")
          .evaluate(Jsons.userMinimal) should be(false)
        parseFilter("bla eq \"urn:ietf:params:scim:schemas:core:2.0:User\"")
          .evaluate(Jsons.userMinimal) should be(false)
      }

      it("should evaluate complex value filter (positive)") {
        parseFilter("userType eq \"Employee\" and emails[type eq \"work\" and value co \"@example.com\"]")
          .evaluate(Jsons.userFull) should be(true)
        parseFilter("userType eq \"Employee\" and emails[type eq \"home\" and value co \"@jensen.org\"]")
          .evaluate(Jsons.userFull) should be(true)
      }
      it("should evaluate complex value filter (negative)") {
        parseFilter("userType eq \"Manager\" and emails[type eq \"work\" and value co \"@example.com\"]")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("userType eq \"Employee\" and emails[type eq \"work\" and value co \"@example.com\"]")
          .evaluate(Jsons.userMinimal) should be(false)
        parseFilter("userType eq \"Employee\" and emails[type eq \"work\" and value co \"@example.org\"]")
          .evaluate(Jsons.userFull) should be(false)
        parseFilter("userType eq \"Employee\" and emails[type eq \"home\" and value co \"@example.com\"]")
          .evaluate(Jsons.userFull) should be(false)
      }
    }

    describe("parseAttributeSelector") {
      it("should parse a simple attribute") {
        parseAS("members") should be(AttributePath("members"))
      }
      it("should parse a subAttribute") {
        parseAS("name.familyName") should be(AttributePath("name", subAttribute = Some("familyName")))
      }
      it("should parse a complex attribute") {
        parseAS("members[value eq\"2819c223-7f76-453a-919d-413861904646\"]") should be(
          FilteredAttributePath(
            "members",
            Equal(AttributePath("value"), StringValue("2819c223-7f76-453a-919d-413861904646")),
          )
        )
      }
      it("should parse a complex attribute with subattribute") {
        parseAS("members[value eq\"2819c223-7f76-453a-919d-413861904646\"].displayName") should be(
          FilteredAttributePath(
            "members",
            Equal(AttributePath("value"), StringValue("2819c223-7f76-453a-919d-413861904646")),
            subAttribute = Some("displayName"),
          )
        )
      }
    }
  }
}
