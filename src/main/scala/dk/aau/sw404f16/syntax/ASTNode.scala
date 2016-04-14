package dk.aau.sw404f16.syntax

import scala.util.parsing.input.Positional

/**
  * Created by coffee on 4/5/16.
  */
trait ASTNode extends Positional {
  type TypeInfo = (String, List[String])
  var typeInfo: TypeInfo = ("", Nil)
}