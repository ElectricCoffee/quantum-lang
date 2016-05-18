package dk.aau.sw404f16.syntax

/**
  * Created by coffee on 28/03/16.
  */
/* case classes (magic classes that work in a switch)
 * override is needed because "data" is already defined in the trait
 * the stuff in the parentheses is the constructor arguments,
 * and in case classes the constructor arguments automatically point to public properties of the same name
 * Case classes also don't need the "new" keyword unlike normal classes
 */
case class Atom(override val data: String) extends Literal with HasData {
  override def toElixir: String = data.replace('#', ':').replace('-', '_')
}

case class BinaryLiteral(override val data: String) extends Literal with HasData {
  override def toElixir: String = Integer.parseInt(data, 2).toString
}
case class HexLiteral(override val data: String) extends Literal with HasData {
  override def toElixir: String = Integer.parseInt(data, 16).toString
}
case class NumberLiteral(override val data: String) extends Literal with HasData {
  override def toElixir: String = data
}
case class Identifier(override val data: String) extends Literal with HasData {
  override def toElixir: String =
    if (Character.isUpperCase(data(0))) // if identifier starts on a capital, make it CamelCase
      data.split('-').map(_.capitalize).mkString("")
    else // otherwise make it snake_case
      data.replace('-', '_')
}
case class Operator(override val data: String) extends Literal with HasData {
  override def toElixir: String = data // TODO: may need to be translated into functions in elixir
}
case class StringLiteral(override val data: String) extends Literal with HasData {
  override def toElixir: String = data
}

/* case objects (magic singletons that work in a switch)
 * case objects are like case classes, except they don't have a constructor
 * they don't have one because only a single instance exists of them
 */
case object Comment extends Literal
case object Assignment extends Literal { // formerly "Equals"
  override def toElixir: String = "="
}
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