package dk.aau.sw404f16.util

import dk.aau.sw404f16.syntax.ASTNode

/**
  * Created by coffee on 4/23/16.
  */
object Convenience {
  /** for when code paths that are impossible to reach,
    * but the compiler won't let you continue without adding the last catch-all case
    */
  def !!! = throw new UnsupportedOperationException("This code shouldn't be reachable")
  /** a pattern so common it might as well be a function */
  def lineRef(node: ASTNode) = s"\"$node\" on line ${node.pos.line}, column ${node.pos.column}"
}