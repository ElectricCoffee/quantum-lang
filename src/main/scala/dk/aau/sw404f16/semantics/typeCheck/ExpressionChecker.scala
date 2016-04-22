package dk.aau.sw404f16.semantics.typeCheck

import dk.aau.sw404f16.semantics.StandardType
import dk.aau.sw404f16.semantics.exceptions.TypeMismatchException
import dk.aau.sw404f16.syntax._
import dk.aau.sw404f16.util.{Bottom, Middle, Top}

/**
  * Created by coffee on 18/04/16.
  */
object ExpressionChecker {
  type TypeInfo = Option[(String, List[String])]
  /** a pattern so common it might as well be a function */
  private def lineRef(node: ASTNode) = s"on line ${node.pos.line}, column ${node.pos.column}"
  /** type-checks the if-statement, making sure the query returns a boolean */
  def checkIfStmt(ifStmt: IfStatement): Either[String, TypeInfo] = ifStmt match {
    case IfStatement(Statement(Middle(valDef)), _) => // value definitions not permitted
      Left(s"$valDef ${lineRef(valDef)} is not a valid expression")
    case IfStatement(Statement(stmt), body) =>
      val expr = stmt match {
        case Top(e) => e
        case Bottom(o) => o
        case Middle(_) => throw new UnsupportedOperationException("Middle should not be reachable here")
      }

      if(checkExpression(expr).contains((StandardType.boolean, Nil)))
        Right(checkExpression(body)) // "right" as in "correct"
      else
        // "left" as in "what's left when you take out the correct"
        Left(s"the expression $expr ${lineRef(expr)} is not of type Bool")
  }

  def checkIfExpr(stmts: List[IfStatement]): TypeInfo = {
    val results = stmts map checkIfStmt
    val errors  = results.filter(x => x.isLeft)
    // TODO: refactor this later
    if(errors.isEmpty) { // if it's empty we only got "Right"s left :D
    // now to make sure all the return types are the same
    val types = results.map(_.right.get)
      val mismatches = types.forall(t => t == types.head)
      if (mismatches)
        throw TypeMismatchException("Not all paths return the same type, please make sure they do so.")
      else types.head
    }
    else { // otherwise round up all the errors and throw them
    val errMsg = errors.map(_.left.get).mkString(", ")
      throw TypeMismatchException(errMsg)
    }
  }

  /**
    * Checks an if-expression to see if all the types are valid
    * the condition should be a boolean
    * and all the return-expressions should return the same type
    *
    * @param ifExpr an IfExpression node in the tree
    */
  @deprecated
  def checkIf(ifExpr: IfExpression): Unit = {
    // check if all the ifs are booleans
    val notBoolean = ifExpr.statements // TODO: refactor later
      .filter(_.boolean.concreteType contains StandardType.boolean)

    if(notBoolean.nonEmpty) {
      val errMsg: List[String] = notBoolean.map { expr =>
        val boolExpr = expr.boolean
        val linum = boolExpr.pos.line
        val conum = boolExpr.pos.column
        s"expression \"$boolExpr\" on line $linum, column $conum is not of type Bool"
      }
      throw TypeMismatchException(errMsg mkString ", ")
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

  /** Checks match-expressions
    * First makes sure that all the things checked on are
    * either the same type as the input,
    * or a subtype of the input.
    * Then it checks to see if all the expressions are the same type also
    *
    * @param matchExpr an instance of a MatchExpression node
    */
  def checkMatch(matchExpr: MatchExpression): Unit = {
    val referenceType = matchExpr.expression.concreteType
    def exceptionHelper(matchStmts: List[MatchStatement]) = {
      val errMsg = matchStmts.map { expr =>
        val pat = expr.patternDefinition
        val pos = pat.pos
        s"The pattern \"${pat.pattern}\" on line ${pos.line}, column ${pos.column} does not match type $referenceType"
      }
      TypeMismatchException(errMsg mkString ", ")
    }

    // create a list of all the expressions that don't match the type
    val mismatchReference = matchExpr.statements.filter(_.patternDefinition.concreteType != referenceType)
    // if that list is empty, set the matchExpression's concrete type to referenceType
    if (mismatchReference.nonEmpty) {
      val mismatchSuper = matchExpr.statements.filter(_.patternDefinition.superType != referenceType)
      if (mismatchSuper.nonEmpty) throw exceptionHelper(mismatchSuper)
      else { // if everything matches, check to see if all the expressions in the match return the same type
        // TODO: Find a better way to do this so excessive iterations aren't used
        // make sure all statements' types are evaluated
        matchExpr.statements.foreach(x => checkExpression(x.body))
        // set the first statement to be the baseline
        val comparable = matchExpr.statements.head.body.concreteType
        // filter out all statements that match, leaving behind all the mismatches
        val mismatches = matchExpr.statements.filter(x => x.body.concreteType != comparable)

        if(mismatches.isEmpty) matchExpr.concreteType = comparable
        else {
          val err = mismatches.map { expr =>
            val pos = expr.body.pos
            s"The expression on line ${pos.line}, column ${pos.column}, doesn't match type $comparable"
          }
          throw TypeMismatchException(err mkString ", ")
        }
      }
    }
    else throw exceptionHelper(mismatchReference)
  }

  def checkExpression(expr: Expression): TypeInfo = expr match {
    case iexpr: IfExpression => ???
    case fexpr: ForComprehension => ???
    case mexpr: MatchExpression => ???
    case liter: Literal => ???
    case aexpr: AskStatement => ???
    case ident: Identifier => ???
    case fcall: FunctionCall => ???
    case fcall: FieldCall => ???
    case mcall: MethodCall => ???
    case unknown => throw new IllegalArgumentException(s"unknown input $unknown")
  }
}