package dk.aau.sw404f16.syntax

import dk.aau.sw404f16.semantics.TypeInfo

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
case class Atom(override val data: String) extends Literal with HasData {
  nodeType = new TypeInfo("Atom") // nodeType defined in ASTNode
}
case class BinaryLiteral(override val data: String) extends Literal with HasData {
  nodeType = new TypeInfo("Binary")
}
case class HexLiteral(override val data: String) extends Literal with HasData {
  nodeType = new TypeInfo("Hexadecimal")
}
case class NumberLiteral(override val data: String) extends Literal with HasData {
  nodeType = TypeInfo.number
}
case class Identifier(override val data: String) extends Literal with HasData // formerly "Name"
case class Operator(override val data: String) extends Literal with HasData
case class StringLiteral(override val data: String) extends Literal with HasData {
  nodeType = TypeInfo.string
}

/* case objects (magic singletons that work in a switch)
 * case objects are like case classes, except they don't have a constructor
 * they don't have one because only a single instance exists of them
 */
case object Assignment extends Literal // formerly "Equals"
case object Comment extends Literal
case object Do extends Literal
case object Yield extends Literal

// these are deprecated because they aren't actually used by the parser
// this might change later though
@deprecated case object About extends Literal
@deprecated case object Actor extends Literal
@deprecated case object Ask extends Literal
@deprecated case object Comma extends Literal
@deprecated case object Data extends Literal
@deprecated case object Define extends Literal
@deprecated case object For extends Literal
@deprecated case object Function extends Literal
@deprecated case object If extends Literal
@deprecated case object Import extends Literal
@deprecated case object In extends Literal
@deprecated case object InheritanceArrow extends Literal
@deprecated case object LeftBrace extends Literal
@deprecated case object LeftBracket extends Literal
@deprecated case object LeftParen extends Literal
@deprecated case object Match extends Literal
@deprecated case object Module extends Literal
@deprecated case object Of extends Literal
@deprecated case object Period extends Literal
@deprecated case object Receiver extends Literal
@deprecated case object RightBrace extends Literal
@deprecated case object RightBracket extends Literal
@deprecated case object RightParen extends Literal
@deprecated case object Semicolon extends Literal
@deprecated case object Tell extends Literal
@deprecated case object Then extends Literal
@deprecated case object Value extends Literal