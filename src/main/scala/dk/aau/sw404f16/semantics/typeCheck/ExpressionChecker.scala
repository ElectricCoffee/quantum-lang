package dk.aau.sw404f16.semantics.typeCheck

import dk.aau.sw404f16.semantics.TypeInfo
import dk.aau.sw404f16.semantics.exceptions.TypeMismatchException
import dk.aau.sw404f16.syntax._
import dk.aau.sw404f16.util.{Bottom, Middle, Top}
import dk.aau.sw404f16.util.Convenience.{!!!, lineRef}

/**
  * Created by coffee on 18/04/16.
  */
object ExpressionChecker {
  def checkExpression(expr: Expression): TypeInfo = {
    // for the sake of performance, don't re-evaluate an expression's type if it already has one
    if(expr.nodeType == null) expr.nodeType = expr match {
      case liter: Literal => liter.nodeType // technically unnecessary
      case BinaryOperation(lhs, op, rhs) => ??? // lookup the operator in the symbol table
      case Block(data) => checkBlock(data)
      case ident: Identifier => ??? // somehow get identifier type from symbol table
      case IfExpression(stmts) => checkIfExpr(stmts)
      case MatchExpression(input, stmts) => checkMatchExpr(input, stmts)
      case ForComprehension(stmts, doOrYield, block) => checkForCompr(stmts, doOrYield, block)
      case AskStatement(targets, messages) => ???
      case FunctionCall(Identifier(id), args) => checkFuncall(id, args)
      case FieldCall(obj, id) => ???
      case MethodCall(obj, FunctionCall(id, args)) => ???
      case unknown =>
        throw new IllegalArgumentException(s"unknown input $unknown")
    }
    expr.nodeType
  }

  /** type-checks the if-statement, making sure the query returns a boolean */
  def checkIfStmt(ifStmt: IfStatement): Either[String, TypeInfo] = ifStmt match {
    case IfStatement(Statement(Middle(valDef)), _) => // value definitions not permitted
      Left(s"$valDef ${lineRef(valDef)} is not a valid expression")
    case IfStatement(Statement(stmt), body) =>
      val expr = stmt match {
        case Top(expression)  => expression
        case Bottom(operator) => operator
        case Middle(_)        => !!! // see util.Convenience for implementation
      }

      if(checkExpression(expr) == TypeInfo.boolean)
        Right(checkExpression(body)) // "right" as in "correct"
      else
        // "left" as in "what's left when you take out the correct"
        Left(s"the expression ${lineRef(expr)} is not of type Bool")
  }

  def checkIfExpr(stmts: List[IfStatement]): TypeInfo = {
    val results = stmts map checkIfStmt
    val errors  = results.filter(_.isLeft)
    // TODO: refactor this later
    if(errors.isEmpty) { // if it's empty we only got "Right"s left :D
    // now to make sure all the return types are the same
    val types = results.map(_.right.get)
      val mismatches = types.forall(_ <=> types.head)
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
  @deprecated def checkIf(ifExpr: IfExpression): Unit = {
    // check if all the ifs are booleans
    val notBoolean = ifExpr.statements // TODO: refactor later
      .filter(_.boolean.nodeType <=> TypeInfo.boolean)

    if(notBoolean.nonEmpty) {
      val errMsg: List[String] = notBoolean.map { expr =>
        s"expression ${lineRef(expr.boolean)} is not of type Bool"
      }
      throw TypeMismatchException(errMsg mkString ", ")
    }

    // check if all the expressions return the same type
    // if they do, the overall type of the if expression is that type
    val types = ifExpr.statements.map(_.body.nodeType)
    val equal = types.forall(_ == types.head)

    if(!equal)
      throw TypeMismatchException("Not all paths return the same type, please make sure they do so.")
    else
      ifExpr.nodeType = types.head
  }

  def checkMatchStmt(matchStmt: MatchStatement): (ASTNode, Expression) = matchStmt match {
    case MatchStatement(PatternDefinition(Left(literal)), expr) =>
      PatternChecker checkPattern literal
       checkExpression(expr)
      (literal, expr)
    case MatchStatement(PatternDefinition(Right(typedVal)), expr) =>
      PatternChecker checkPattern typedVal
      checkExpression(expr)
      (typedVal, expr)
  }

  def checkMatchExpr(input: Expression, stmts: List[MatchStatement]): TypeInfo = {
    import dk.aau.sw404f16.util.Extensions.RichTupleList // imports .toTuple
    val (patterns, expressions) = stmts.map(checkMatchStmt).toTuple
    val retType = expressions.head.nodeType

    val patErrors = patterns
      .filter(pat => pat.nodeType <!=> input.nodeType || pat.nodeType <!^=> input.nodeType)
      .map(pat => s"pattern $pat ${lineRef(pat)} does not match type ${input.nodeType}")

    val exprErrors = expressions
      .filter(exp => exp.nodeType <!=> retType)
      .map(exp => s"expressions $exp ${lineRef(exp)} does not match return type $retType")

    val err = (patErrors ++ exprErrors).mkString(", ")

    if (err.nonEmpty) throw TypeMismatchException(err)
    else retType
  }

  /** Checks match-expressions
    * First makes sure that all the things checked on are
    * either the same type as the input,
    * or a subtype of the input.
    * Then it checks to see if all the expressions are the same type also
    *
    * @param matchExpr an instance of a MatchExpression node
    */
  @deprecated def checkMatch(matchExpr: MatchExpression): Unit = {
    val referenceType = matchExpr.expression.nodeType
    def exceptionHelper(matchStmts: List[MatchStatement]) = {
      val errMsg = matchStmts.map { expr =>
        val pat = expr.patternDefinition
        s"The pattern \"${pat.pattern}\" ${lineRef(pat)} does not match type $referenceType"
      }
      TypeMismatchException(errMsg mkString ", ")
    }

    // create a list of all the expressions that don't match the type
    val mismatchReference = matchExpr.statements.filter(_.patternDefinition.nodeType <!=> referenceType)
    // if that list is empty, set the matchExpression's concrete type to referenceType
    if (mismatchReference.nonEmpty) {
      val mismatchSuper = matchExpr.statements.filter(_.patternDefinition.nodeType <!^=> referenceType)
      if (mismatchSuper.nonEmpty) throw exceptionHelper(mismatchSuper)
      else { // if everything matches, check to see if all the expressions in the match return the same type
        // TODO: Find a better way to do this so excessive iterations aren't used
        // make sure all statements' types are evaluated
        matchExpr.statements.foreach(x => checkExpression(x.body))
        // set the first statement to be the baseline
        val comparable = matchExpr.statements.head.body.nodeType
        // filter out all statements that match, leaving behind all the mismatches
        val mismatches = matchExpr.statements.filter(x => x.body.nodeType <!=> comparable)

        if(mismatches.isEmpty) matchExpr.nodeType = comparable
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

  def checkForStmt(forStmt: ForStatement): Identifier = forStmt match {
    case ForStatement(ident, expr) =>
      if(expr.nodeType <=> TypeInfo.list) {
        // TODO: lookup and add identifier with type to symbol table
        Identifier("placeholder")
      }
      else throw TypeMismatchException(s"expression ${lineRef(expr)} is not of type ${TypeInfo.list}")
  }

  def checkForCompr(forStmts: List[ForStatement], doOrYield: Either[Do.type, Yield.type], block: Block): TypeInfo =
    doOrYield match {
      case Left(Do) =>
        // TODO: add a way to reference all the created variables in the for statements
        TypeInfo.unit
      case Right(Yield) =>
        // TODO: same as above
        checkExpression(block)
    }

  def checkFuncall(id: String, args: List[Expression]): TypeInfo = {
    // lookup function id in symbol table, and get its type
    // make sure the function arguments match the inputs as dictated by the symbol table

    new TypeInfo("Placeholder")
  }

  def checkBlock(statements: List[Statement]): TypeInfo = {
    statements.foreach(StatementChecker.checkStatement) // evaluate all statements to give them their type
    statements.last.nodeType // last statement in a block is the block's return-type
  }
}