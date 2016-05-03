package dk.aau.sw404f16.semantics.typeCheck

import dk.aau.sw404f16.semantics.TypeInfo
import dk.aau.sw404f16.syntax.Statement
import dk.aau.sw404f16.util.{Bottom, Middle, Top}

/**
  * Created by coffee on 27/04/16.
  */
object StatementChecker {
  def checkStatement(statement: Statement): TypeInfo = {
    val Statement(stmt) = statement
    stmt match {
      case Top(expr) => ExpressionChecker check expr
      case Middle(valDef) =>
        // TODO: store the value's data and type in the symbol table
        TypeInfo.unit // value definitions have no type
      case Bottom(binOp) => ExpressionChecker check binOp
    }
  }
}
