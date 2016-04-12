package dk.aau.sw404f16.syntax

/**
  * Created by coffee on 28/03/16.
  */
// traits (scala's interfaces)
trait Terminal extends ASTNode{
  // objects don't have constructors, so data is empty by default
  val data: String = ""
}

/* case classes (magic classes that work in a switch)
 * override is needed because "data" is already defined in the trait
 * the stuff in the parentheses is the constructor arguments,
 * and in case classes the constructor arguments automatically point to public properties of the same name
 * Case classes also don't need the "new" keyword unlike normal classes
 */
case class Atom(override val data: String) extends Terminal
case class BinaryLiteral(override val data: String) extends Terminal
case class HexLiteral(override val data: String) extends Terminal
case class NumberLiteral(override val data: String) extends Terminal
case class Identifier(override val data: String) extends Terminal // formerly "Name"
case class Operator(override val data: String) extends Terminal
case class StringLiteral(override val data: String) extends Terminal

/* case objects (magic singletons that work in a switch)
 * case objects are like case classes, except they don't have a constructor
 * they don't have one because only a single instance exists of them
 */
case object About extends Terminal
case object Actor extends Terminal
case object Ask extends Terminal
case object Comma extends Terminal
case object Data extends Terminal
case object Define extends Terminal
case object Assignment extends Terminal // formerly "Equals"
case object For extends Terminal
case object Do extends Terminal
case object Yield extends Terminal
case object Function extends Terminal
case object If extends Terminal
case object Import extends Terminal
case object In extends Terminal
case object InheritanceArrow extends Terminal
case object LeftBrace extends Terminal
case object LeftBracket extends Terminal
case object LeftParen extends Terminal
case object Match extends Terminal
case object Module extends Terminal
case object Of extends Terminal
case object Period extends Terminal
case object Receiver extends Terminal
case object RightBrace extends Terminal
case object RightBracket extends Terminal
case object RightParen extends Terminal
case object Semicolon extends Terminal
case object Tell extends Terminal
case object Then extends Terminal
case object Value extends Terminal