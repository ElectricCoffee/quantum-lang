package dk.aau.sw404f16.syntax

import scala.util.parsing.input.Positional
import dk.aau.sw404f16.semantics.TypeInfo

/**
  * Created by coffee on 4/5/16.
  */
trait ASTNode extends Positional {
  var nodeType: TypeInfo = null
}