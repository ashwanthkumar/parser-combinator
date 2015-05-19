package in.ashwanthkumar.parser

import org.scalatest.FlatSpec
import org.scalatest.Matchers.{convertToAnyShouldWrapper, be}

class ParserTest extends FlatSpec {
  "Parser" should "parse a char as given" in {
    Parser.char('a').parse("abcd").^^{ x => x.toString}.get should be("a")
    Parser.char('a').parse("abcd").get should be('a')

    val parseB = Parser.char('b')
    parseB.parse("abcd") should be(parseB.Failure("b expected but a found", "abcd"))
  }

  it should "parse a literal string" in {
    val literalParser = Parser.literal("abcd")
    literalParser.parse("abcdefgh") should be(literalParser.Success("abcd", "efgh"))
  }
}
