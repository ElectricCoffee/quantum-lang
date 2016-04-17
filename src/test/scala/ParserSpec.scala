import org.scalatest._
import scala.util.parsing.combinator._
import dk.aau.sw404f16.syntax.parser.{Parser => NParser}
import dk.aau.sw404f16.syntax.{BinaryOperation, Identifier, IfExpression, IfStatement, Operator, Statement, StringLiteral}
import dk.aau.sw404f16.util.Bottom
import dk.aau.sw404f16.syntax.NumberLiteral

/**
  * Created by coffee on 3/30/16.
  * Documentation for ScalaTest available on http://www.scalatest.org/
  */
class ParserSpec extends FlatSpec with Matchers with RegexParsers {

  it should "parse a string as a valid stringliteral" in {
    val input = "\"hej med dig\""
    var result = NParser.parse(NParser.stringLiteral, input)
    val expectation = StringLiteral(input)
    result should be (expectation)

  }

  it should "parse a string as a valid numberliteral" in {
    val input = "1234"
    var result = NParser.parse(NParser.numberLiteral, input)
    val expectation = NumberLiteral("1234")
    result should be (expectation)
  }

  it should "Parse a string as a valid if statement" in {

    val input =
      """
        |if a > b then "hia"
      """.stripMargin

    var result = NParser.parse(NParser.ifExpr, input)

     val expectation = IfExpression(List(IfStatement(Statement(Bottom(BinaryOperation(Identifier("a"), Operator(">"), Identifier("b")))), StringLiteral("\"hia\""))))

    result should be (expectation)
  }
}