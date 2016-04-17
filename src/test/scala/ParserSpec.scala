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
  "The Parser" should "parse a string as an operator" in {
    val inputs = List("<", ">", "==", ">>=", "<*>", "<|>", "->")
    val expected = List(Operator("<"), Operator(">"), Operator("=="), Operator(">>="), Operator("<*>"), Operator("<|>"), Operator("->"))

    val result = inputs.map(input => NParser.parse(NParser.operator, input))
    result should be (expected)
  }

  it should "parse a string as a valid stringliteral" in {
    val input = "\"hej med dig\""
    val expectation = StringLiteral(input)

    val result = NParser.parse(NParser.stringLiteral, input)
    result should be (expectation)
  }

  it should "parse a string as a valid numberliteral" in {
    val input = "1234"
    val expectation = NumberLiteral("1234")

    val result = NParser.parse(NParser.numberLiteral, input)
    result should be (expectation)
  }

  it should "Parse a string as a valid if statement" in {

    val input =
      """
        |if {
        |  a > b then "hia";
        |  c < d then "hello";
        |  p == q then "hi";
        |}
      """.stripMargin

    val case1 = IfStatement(Statement(Bottom(BinaryOperation(Identifier("a"), Operator(">"), Identifier("b")))), StringLiteral("\"hia\""))
    val case2 = IfStatement(Statement(Bottom(BinaryOperation(Identifier("c"), Operator("<"), Identifier("d")))), StringLiteral("\"hello\""))
    val case3 = IfStatement(Statement(Bottom(BinaryOperation(Identifier("p"), Operator("=="), Identifier("q")))), StringLiteral("\"hi\""))
    
    val expectation = IfExpression(List(case1, case2, case3))
    val result = NParser.parse(NParser.ifExpr, input)
    result should be (expectation)
  }
}