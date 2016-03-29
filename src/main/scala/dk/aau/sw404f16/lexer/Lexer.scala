package dk.aau.sw404f16.lexer

import jdk.nashorn.internal.runtime.regexp.joni.exception.SyntaxException
import Regexp._

/**
  * Created by coffee on 28/03/16.
  */
object Lexer {
  // make this prettier later
  def removeSpaces(input: Seq[String]): Seq[String] = {
    val head :: tail = input
    if(tail != Nil) head match {
      case " " | "" => removeSpaces(tail)
      case _   => head +: removeSpaces(tail)
    } else input
  }

  def createTokenList(input: String): List[Token] =
    removeSpaces(input.split("(\\s|\\b)").toList)
      .map(tokenizeString)
      .toList

  def tokenizeString(input: String): Token = input match {
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
