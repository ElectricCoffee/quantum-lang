package dk.aau.sw404f16.lexer

/**
  * Created by coffee on 28/03/16.
  */
// traits (scala's interfaces)
trait Token {
  // objects don't have constructors, so data is empty by default
  val data: String = ""
}

/* case classes (magic classes that work in a switch)
 * override is needed because "data" is already defined in the trait
 * the stuff in the parentheses is the constructor arguments,
 * and in case classes the constructor arguments automatically point to public properties of the same name
 * Case classes also don't need the "new" keyword unlike normal classes
 */
case class BinaryLiteral(override val data: String) extends Token
case class HexLiteral(override val data: String) extends Token
case class NumberLiteral(override val data: String) extends Token
case class Identifier(override val data: String) extends Token // formerly "Name"
case class Operator(override val data: String) extends Token
case class StringLiteral(override val data: String) extends Token

/* case objects (magic singletons that work in a switch)
 * case objects are like case classes, except they don't have a constructor
 * they don't have one because only a single instance exists of them
 */
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