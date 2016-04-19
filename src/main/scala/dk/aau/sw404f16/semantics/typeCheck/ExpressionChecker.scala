package dk.aau.sw404f16.semantics.typeCheck

import dk.aau.sw404f16.semantics.StandardType
import dk.aau.sw404f16.semantics.exceptions.TypeMismatchException
import dk.aau.sw404f16.syntax.{IfExpression, MatchExpression, MatchStatement}

/**
  * Created by coffee on 18/04/16.
  */
object ExpressionChecker {
  type TypeInfo = Option[(String, List[String])]
  /**
    * Checks an if-expression to see if all the types are valid
    * the condition should be a boolean
    * and all the return-expressions should return the same type
    * @param ifExpr an IfExpression node in the tree
    */
  def checkIf(ifExpr: IfExpression): Unit = {
    // check if all the ifs are booleans
    val notBoolean = ifExpr.statements
      .filter(_.boolean.concreteType contains StandardType.boolean)

    if(notBoolean.nonEmpty) {
      val errMsg: List[String] = notBoolean.map { expr =>
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
    val referenceType = matchExpr.expression.concreteType
    def exceptionHelper(matchStmts: List[MatchStatement]) = {
      val errMsg = matchStmts.map { expr =>
        val pattern    = expr.patternDefinition
        val line       = pattern.pos.line
        val column     = pattern.pos.column
        val expression = pattern.pattern
        s"The expression \"$expression\" on line $line, column $column does not match type $referenceType"
      }
      TypeMismatchException(errMsg mkString ", ")
    }

    // create a list of all the expressions that don't match the type
    val mismatchReference = matchExpr.statements.filter(_.patternDefinition.concreteType != referenceType)
    // if that list is empty, set the matchExpression's concrete type to referenceType
    if (mismatchReference.nonEmpty) {
      val mismatchSuper = matchExpr.statements.filter(_.patternDefinition.superType != referenceType)
      if (mismatchSuper.nonEmpty) throw exceptionHelper(mismatchSuper)
      else {
        // do shit
      }
    }
    else throw exceptionHelper(mismatchReference)
  }
}
