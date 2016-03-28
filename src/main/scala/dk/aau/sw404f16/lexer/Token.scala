package dk.aau.sw404f16.lexer

/**
  * Created by coffee on 28/03/16.
  */
// traits
trait Token {
  // objects don't have constructors, so data is empty by default
  val data: String = ""
}

// case classes
case class BinaryLiteral(override val data: String) extends Token
case class HexLiteral(override val data: String) extends Token
case class NumberLiteral(override val data: String) extends Token
case class Identifier(override val data: String) extends Token // formerly "Name"
case class Operator(override val data: String) extends Token
case class StringLiteral(override val data: String) extends Token

// case objects
case object About extends Token
case object Actor extends Token
case object Ask extends Token
case object Atom extends Token
case object Comma extends Token
case object Data extends Token
case object Define extends Token
case object Equals extends Token
case object For extends Token
case object Function extends Token
case object If extends Token
case object Import extends Token
case object In extends Token
case object InheritanceArrow extends Token
case object LeftBrace extends Token
case object LeftBracket extends Token
case object LeftParen extends Token
case object Match extends Token
case object Module extends Token
case object Of extends Token
case object Period extends Token
case object Receiver extends Token
case object RightBrace extends Token
case object RightBracket extends Token
case object RightParen extends Token
case object Semicolon extends Token
case object Tell extends Token
case object Then extends Token
case object Token extends Token
case object Value extends Token