import org.scalatest._
import scala.util.parsing.combinator._
import dk.aau.sw404f16.syntax.parser.{Parser => NParser}
import dk.aau.sw404f16.syntax.{BinaryOperation, Identifier, IfExpression, IfStatement, Operator, Statement, StringLiteral}
import dk.aau.sw404f16.util.Bottom

/**
  * Created by coffee on 3/30/16.
  * Documentation for ScalaTest available on http://www.scalatest.org/
  */
class ParserSpec extends FlatSpec with Matchers with RegexParsers {
  "The parser" should "parse a string as a valid identifier" in {


  }
  it should "parse a string as a valid stringliteral" in {


  }
  it should "parse a string as a valid numberliteral" in {


  }
  it should "parse a string as a valid assignment" in {


  }






  it should "Parse a string as a valid if statement" in {

    val input =
      """
        |if a > b then "hia";
      """.stripMargin

    var result = NParser.parse(NParser.ifExpr, input)

     val expectation = IfExpression(List(IfStatement(Statement(Bottom(BinaryOperation(Identifier("a"), Operator(">"), Identifier("b")))), StringLiteral("hia"))))

    result should be (expectation)
  }

}