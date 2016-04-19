package dk.aau.sw404f16.semantics.typeCheck

import dk.aau.sw404f16.semantics.StandardType
import dk.aau.sw404f16.semantics.exceptions.TypeMismatchException
import dk.aau.sw404f16.syntax.{IfExpression, MatchExpression}

/**
  * Created by coffee on 18/04/16.
  */
object ExpressionChecker {
  def checkIf(ifExpr: IfExpression): Unit = {
    // check if all the ifs are booleans
    val notBoolean = ifExpr
      .statements
      .filter(_.boolean.concreteType contains StandardType.boolean)

    if(notBoolean.nonEmpty) {
      val errMsg = notBoolean.map { expr =>
        val boolExpr = expr.boolean
        val linum = boolExpr.pos.line
        val conum = boolExpr.pos.column
        s"expression \"$boolExpr\" on line $linum, column $conum is not of type Bool"
      }
      throw new TypeMismatchException(errMsg mkString ", ")
    }

    // check if all the expressions return the same type
    // if they do, the overall type of the if expression is that type
    val types = ifExpr.statements.map(_.body.concreteType)
    val equal = types.forall(_ == types.head)

    if(!equal)
      throw TypeMismatchException("Not all paths return the same type, please make sure they do so.")
    else
      ifExpr.concreteType = types.head
  }

  def checkMatch(matchExpr: MatchExpression): Unit = {

  }
}
