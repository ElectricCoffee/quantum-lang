package dk.aau.sw404f16.semantics.typeCheck

import dk.aau.sw404f16.semantics.TypeInfo
import dk.aau.sw404f16.syntax.{Literal, TypedValue}

/**
  * Created by coffee on 4/25/16.
  */
object PatternChecker {
  def checkPattern(pat: TypedValue): TypeInfo = ??? // refer to symbol table somehow
  def checkPattern(pat: Literal): TypeInfo = pat.nodeType
}
