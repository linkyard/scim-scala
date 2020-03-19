package scim.model

import java.net.URI
import io.circe.Json
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers
import scim.model.Arbitraries._
import scim.model.Filter.{And, AttributePath, Comparison, Or}

class FilterSpec extends AnyFunSpec with Checkers with Matchers {

  describe("Filter") {
    describe("parse") {
      //      it("and asString should be compatible")(check { filter: Filter =>
      //        val string = filter.asString
      //        println(string)
      //        Filter.parse(string) == Right(filter)
      //      })
      //
      //      it("should be default for empty") {
      //        Filter.parse("") should be(Right(Filter.none))
      //        Filter.parse(" ") should be(Right(Filter.none))
      //      }
      //
      //      it("should be left from invalid") {
      //        Filter.parse("bla").isLeft should be(true)
      //        Filter.parse("hube").isLeft should be(true)
      //      }

      def parseSuccessful(value: String): Filter = {
        Filter.parse(value) match {
          case Right(result) => result
          case Left(error) => fail(s"parsing of '$value' failed: ${error}")
        }
      }

      it("should parse simple eq filter") {
        val r = parseSuccessful("userName eq \"bjensen\"")
        r should be(Comparison.Equal(AttributePath("userName"), Json.fromString("bjensen")))
      }
      it("should parse subpath co filter") {
        val r = parseSuccessful("name.familyName co \"O'Malley\"")
        r should be(Comparison.Contains(AttributePath("name", subAttribute = Some("familyName")), Json.fromString("O'Malley")))
      }
      it("should parse simple sw filter") {
        val r = parseSuccessful("userName sw \"J\"")
        r should be(Comparison.StartsWith(AttributePath("userName"), Json.fromString("J")))
      }
      it("should parse sw filter with uri") {
        val r = parseSuccessful("urn:ietf:params:scim:schemas:core:2.0:User:userName sw \"J\"")
        r should be(Comparison.StartsWith(
          AttributePath("userName", uri = Some(URI.create("urn:ietf:params:scim:schemas:core:2.0:User"))),
          Json.fromString("J")))
      }
      it("should parse simple pr filter") {
        val r = parseSuccessful("title pr")
        r should be(Comparison.Present(AttributePath("title")))
      }
      it("should parse a gt filter") {
        val r = parseSuccessful("meta.lastModified gt \"2011-05-13T04:42:34Z\"")
        r should be(Comparison.GreaterThan(AttributePath("meta", subAttribute = Some("lastModified")), Json.fromString("2011-05-13T04:42:34Z")))
      }
      it("should parse a ge filter") {
        val r = parseSuccessful("meta.lastModified ge \"2011-05-13T04:42:34Z\"")
        r should be(Comparison.GreaterThanOrEqual(AttributePath("meta", subAttribute = Some("lastModified")), Json.fromString("2011-05-13T04:42:34Z")))
      }
      it("should parse a lt filter") {
        val r = parseSuccessful("meta.lastModified lt \"2011-05-13T04:42:34Z\"")
        r should be(Comparison.LessThan(AttributePath("meta", subAttribute = Some("lastModified")), Json.fromString("2011-05-13T04:42:34Z")))
      }
      it("should parse a le filter") {
        val r = parseSuccessful("meta.lastModified le \"2011-05-13T04:42:34Z\"")
        r should be(Comparison.LessThanOrEqual(AttributePath("meta", subAttribute = Some("lastModified")), Json.fromString("2011-05-13T04:42:34Z")))
      }
      it("should parse an 'and' combination") {
//        val r = parseSuccessful("title pr and userType eq \"Employee\"")
        val r = parseSuccessful("(title pr) and (userType eq \"Employee\")")
        r should be(
          And(
            Comparison.Present(AttributePath("title")),
            Comparison.Equal(AttributePath("userType"), Json.fromString("Employee")),
          ))
      }
      it("should parse an 'or' combination") {
        val r = parseSuccessful("title pr or userType eq \"Intern\"")
        r should be(
          Or(
            Comparison.Present(AttributePath("title")),
            Comparison.Equal(AttributePath("userType"), Json.fromString("Intern")),
          ))
      }
      it("should parse another eq filter") {
        val r = parseSuccessful("schemas eq \"urn:ietf:params:scim:schemas:extension:enterprise:2.0:User\"")
        r should be(Comparison.Equal(AttributePath("schemas"), Json.fromString("urn:ietf:params:scim:schemas:extension:enterprise:2.0:User")))
      }

      // TODO
      //      filter=userType eq "Employee" and (emails co "example.com" or
      //        emails.value co "example.org")
      //
      // TODO
      //      filter=userType ne "Employee" and not (emails co "example.com" or
      //        emails.value co "example.org")
      //
      // TODO
      //      filter=userType eq "Employee" and (emails.type eq "work")
      //
      // TODO
      //      filter=userType eq "Employee" and emails[type eq "work" and
      //        value co "@example.com"]
      //
      // TODO
      //      filter=emails[type eq "work" and value co "@example.com"] or
      //      ims[type eq "xmpp" and value co "@foo.com"]
    }
  }

}
