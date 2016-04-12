package dk.aau.sw404f16.syntax.lexer

import jdk.nashorn.internal.runtime.regexp.joni.exception.SyntaxException
import Regexp._
import dk.aau.sw404f16.syntax._
import dk.aau.sw404f16.util.Extensions.RichStringList
/**
  * Created by coffee on 28/03/16.
  */
object Lexer {
  def createTokenList(input: String): List[ASTNode] =
    input.split("(\\s|\\b)").toList
      .stripSpaces // defined in util/Extensions/RichStringList
      .map(tokenizeString)

  def tokenizeString(input: String): ASTNode = input match {
    // parameter-less tokens
    case aboutTok()     => About
    case actorTok()     => Actor
    case askTok()       => Ask
    case assignTok()    => Assignment
    case commaTok()     => Comma
    case dataTok()      => Data
    case defineTok()    => Define
    case forTok()       => For
    case funcTok()      => Function
    case ifTok()        => If
    case importTok()    => Import
    case inTok()        => In
    case inheritTok()   => InheritanceArrow
    case lBraceTok()    => LeftBrace
    case lBracketTok()  => LeftBracket
    case lParenTok()    => LeftParen
    case matchTok()     => Match
    case moduleTok()    => Module
    case ofTok()        => Of
    case periodTok()    => Period
    case receiverTok()  => Receiver
    case rBraceTok()    => RightBrace
    case rBracketTok()  => RightBracket
    case rParenTok()    => RightParen
    case semicolonTok() => Semicolon
    case tellTok()      => Tell
    case thenTok()      => Then
    case valueTok()     => Value

    // parameterized Tokens
    case atomTok(a)      => Atom(a)
    case binLitTok(b)    => BinaryLiteral(b)
    case hexLitTok(x)    => HexLiteral(x)
    case numLitTok(n)    => NumberLiteral(n)
    case operatorTok(o)  => Operator(o)
    case stringLitTok(s) => StringLiteral(s)
    case idTok(i)        => Identifier(i)

    // error
    case unknownToken => throw new SyntaxException(s"Invalid Token: $unknownToken")
  }
}
