import dk.aau.sw404f16.syntax._
import dk.aau.sw404f16.syntax.lexer._
import jdk.nashorn.internal.runtime.regexp.joni.exception.SyntaxException
import org.scalatest._
/**
  * Created by coffee on 28/03/16.
  * Documentation for ScalaTest available on http://www.scalatest.org/
  */
class TokenizerSpec extends FlatSpec with Matchers {
  val addExample = List(Function, Identifier("add"), LeftParen,
    Identifier("Num"), Identifier("x"), Comma, Identifier("Num"), Identifier("y"), RightParen, Assignment,
    Identifier("x"), Operator("+"), Identifier("y"), Semicolon)

  val tellExample = List(Tell, Identifier("stack"), About, Atom("push"),
    LeftParen, StringLiteral("\"the man said \\\"hello\\\"\""), RightParen, Semicolon)

  "The Lexer" should "translate string inputs to objects of the Token trait" in {
    val test1 = List("tell", "stack", "about", "#push", "(", "\"the man said \\\"hello\\\"\"", ")", ";")
    val test2 = List("func", "add", "(", "Num", "x", ",", "Num", "y", ")", "=", "x", "+", "y", ";")
    val test3 = "0b1101111010101101" // 0xDEAD in hex
    val test4 = "0xbadf00d"

    test1.map(Lexer.tokenizeString) should be (tellExample)

    test2.map(Lexer.tokenizeString) should be (addExample)

    Lexer.tokenizeString(test3) should be (BinaryLiteral("1101111010101101"))

    Lexer.tokenizeString(test4) should be (HexLiteral("badf00d"))
  }

  it should "throw SyntaxException if fed a string it doesn't recognise" in {
    val garbage = "sdflk#$!#SD!#"

    a [SyntaxException] should be thrownBy {
      Lexer.tokenizeString(garbage)
    }
  }

  it should "parse an uninterrupted string into a list of tokens" in {
    val codeString = "func add(Num x, Num y) = x + y;"

    Lexer.createTokenList(codeString) should be (addExample)
  }
}
