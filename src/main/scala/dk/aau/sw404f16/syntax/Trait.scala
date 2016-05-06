package dk.aau.sw404f16.syntax

import dk.aau.sw404f16.semantics.TypeInfo

import scala.util.parsing.input.Positional

/**
  * Created by coffee on 5/6/16.
  */
// traits (scala's interfaces)
trait HasData {
  val data: String = ""
}
trait ASTNode extends Positional {
  var nodeType: TypeInfo = null
}
trait TopLevelCons extends ASTNode
trait ActorVariant extends TopLevelCons
trait Expression extends ASTNode
trait Literal extends Expression