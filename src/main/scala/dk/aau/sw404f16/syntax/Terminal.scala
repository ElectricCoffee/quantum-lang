package dk.aau.sw404f16.syntax

/**
  * Created by coffee on 28/03/16.
  */
// traits (scala's interfaces)
trait HasData {
  val data: String = ""
}

/* case classes (magic classes that work in a switch)
 * override is needed because "data" is already defined in the trait
 * the stuff in the parentheses is the constructor arguments,
 * and in case classes the constructor arguments automatically point to public properties of the same name
 * Case classes also don't need the "new" keyword unlike normal classes
 */
case class Atom(override val data: String) extends Literal with HasData
case class BinaryLiteral(override val data: String) extends Literal with HasData
case class HexLiteral(override val data: String) extends Literal with HasData
case class NumberLiteral(override val data: String) extends Literal with HasData
case class Identifier(override val data: String) extends Literal with HasData // formerly "Name"
case class Operator(override val data: String) extends Literal with HasData
case class StringLiteral(override val data: String) extends Literal with HasData

/* case objects (magic singletons that work in a switch)
 * case objects are like case classes, except they don't have a constructor
 * they don't have one because only a single instance exists of them
 */
case object About extends Literal
case object Actor extends Literal
case object Ask extends Literal
case object Comma extends Literal
case object Data extends Literal
case object Define extends Literal
case object Assignment extends Literal // formerly "Equals"
case object For extends Literal
case object Do extends Literal
case object Yield extends Literal
case object Function extends Literal
case object If extends Literal
case object Import extends Literal
case object In extends Literal
case object InheritanceArrow extends Literal
case object LeftBrace extends Literal
case object LeftBracket extends Literal
case object LeftParen extends Literal
case object Match extends Literal
case object Module extends Literal
case object Of extends Literal
case object Period extends Literal
case object Receiver extends Literal
case object RightBrace extends Literal
case object RightBracket extends Literal
case object RightParen extends Literal
case object Semicolon extends Literal
case object Tell extends Literal
case object Then extends Literal
case object Value extends Literal