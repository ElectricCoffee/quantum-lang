import org.scalatest._
import scala.util.parsing.combinator._



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

    var result = parseAll(Parser.ifExpr, input ) match {
      case Succes(result, _) => result
      case failure : NoSuccess => scala.sys.error(failure.msg)
    }
  }

}