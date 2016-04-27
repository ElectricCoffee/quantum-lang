package dk.aau.sw404f16.semantics.typeCheck

import dk.aau.sw404f16.semantics.TypeInfo
import dk.aau.sw404f16.syntax.{BinaryOperation, Expression, ValueDefinition}
import dk.aau.sw404f16.util.{Bottom, Either3, Middle, Top}

/**
  * Created by coffee on 27/04/16.
  */
object StatementChecker {
  def checkStatement(stmt: Either3[Expression, ValueDefinition, BinaryOperation]): TypeInfo = stmt match {
    case Top(expr)      => ExpressionChecker checkExpression expr
    case Middle(valDef) =>
      // TODO: store the value's data and type in the symbol table
      TypeInfo.unit // value definitions have no type
    case Bottom(binOp)  => ExpressionChecker checkExpression binOp
  }
}
