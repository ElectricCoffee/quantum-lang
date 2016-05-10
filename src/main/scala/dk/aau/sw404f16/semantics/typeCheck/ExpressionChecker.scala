package dk.aau.sw404f16.semantics.typeCheck

import dk.aau.sw404f16.semantics.TypeInfo
import dk.aau.sw404f16.semantics.exceptions.TypeMismatchException
import dk.aau.sw404f16.syntax._
import dk.aau.sw404f16.util.{Bottom, Middle, Top}
import dk.aau.sw404f16.util.Convenience.{!!!, lineRef}
import dk.aau.sw404f16.util.Extensions.{RichTupleList, RichASTNodeList}

/**
  * Created by coffee on 18/04/16.
  */
object ExpressionChecker {
  def check(expr: Expression): TypeInfo = {
    // for the sake of performance, don't re-evaluate an expression's type if it already has one
    if(expr.nodeType == null) expr.nodeType = expr match {
      case liter: Literal => liter.nodeType // technically unnecessary
      case BinaryOperation(lhs, op, rhs) => checkBinOp(lhs, op, rhs)
      case Block(data) => checkBlock(data)
      case IfExpression(stmts) => checkIfExpr(stmts)
      case MatchExpression(input, stmts) => checkMatchExpr(input, stmts)
      case ForComprehension(stmts, doOrYield, block) => checkForCompr(stmts, doOrYield, block)
      case AskStatement(targets, messages) => checkAskExpr(targets, messages)
      case FunctionCall(Identifier(id), args) => checkFuncall(id, args) // might be a special case of method-call
      case MethodCall(obj, FunctionCall(id, args)) => checkMethodCall(obj, id, args)
      case FieldCall(obj, id) => checkFieldCall(obj, id) // might ALSO be a special case of method-call
      case value: Identifier => checkValue(value) // somehow get identifier type from symbol table, maybe also special case
      case unknown =>
        throw new IllegalArgumentException(s"unknown input $unknown")
    }
    expr.nodeType
  }

  private def checkBinOp(lhs: Expression, operator: Operator, rhs: Expression): TypeInfo = {
    val leftHandSideType  = check(lhs)
    val rightHandSideType = check(rhs)
    // TODO: lookup the operator in the symbol table, and compare its input types to the hrs and lhs
    new TypeInfo("Placeholder")
  }

  /** type-checks the if-statement, making sure the query returns a boolean */
  private def checkIfStmt(ifStmt: IfStatement): Either[String, TypeInfo] = ifStmt match {
    case IfStatement(Statement(Middle(valDef)), _) => // value definitions not permitted
      Left(s"$valDef ${lineRef(valDef)} is not a valid expression")
    case IfStatement(Statement(stmt), body) =>
      val expr = stmt match {
        case Top(expression)  => expression
        case Bottom(operator) => operator
        case Middle(_)        => !!! // see util.Convenience for implementation
      }

      if(check(expr) == TypeInfo.boolean)
        Right(check(body)) // "right" as in "correct"
      else
        // "left" as in "what's left when you take out the correct"
        Left(s"the expression ${lineRef(expr)} is not of type Bool")
  }

  private def checkIfExpr(stmts: List[IfStatement]): TypeInfo = {
    val results = stmts map checkIfStmt
    val errors  = results.filter(_.isLeft)
    // TODO: refactor this later
    if(errors.isEmpty) { // if it's empty we only got "Right"s left :D
    // now to make sure all the return types are the same
    val types = results.map(_.right.get)
      val mismatches = types.forall(_ |=| types.head)
      if (mismatches)
        throw TypeMismatchException("Not all paths return the same type, please make sure they do so.")
      else types.head
    }
    else { // otherwise round up all the errors and throw them
      val errMsg = errors.map(_.left.get).mkString(", ")
      throw TypeMismatchException(errMsg)
    }
  }

  private def checkMatchStmt(matchStmt: MatchStatement): (ASTNode, Expression) = matchStmt match {
    case MatchStatement(PatternDefinition(Left(literal)), expr) =>
      PatternChecker checkPattern literal
       check(expr)
      (literal, expr)
    case MatchStatement(PatternDefinition(Right(typedVal)), expr) =>
      PatternChecker checkPattern typedVal
      check(expr)
      (typedVal, expr)
  }

  private def checkMatchExpr(input: Expression, stmts: List[MatchStatement]): TypeInfo = {
    val (patterns, expressions) = stmts.map(checkMatchStmt).toTuple
    val retType = expressions.head.nodeType

    val throwPatternMismatches = patterns.throwIfMismatch( pat =>
      (pat.nodeType |!=| input.nodeType) || (pat.nodeType |!?=| input.nodeType)
    )(_)
    val throwExprMismatches = expressions.throwIfMismatch(exp => exp.nodeType |!=| retType)(_)

    throwPatternMismatches(pat => s"pattern ${lineRef(pat)} does not match type ${input.nodeType}")
    throwExprMismatches(exp => s"expressions ${lineRef(exp)} does not match return type $retType")
    retType
  }

  private def checkForStmt(forStmt: ForStatement): Identifier = forStmt match {
    case ForStatement(ident, expr) =>
      if(expr.nodeType |=| TypeInfo.lst) {
        // TODO: lookup and add identifier with type to symbol table
        Identifier("placeholder")
      }
      else throw TypeMismatchException(s"expression ${lineRef(expr)} is not of type ${TypeInfo.lst}")
  }

  private def checkForCompr(forStmts: List[ForStatement], doOrYield: Either[Do.type, Yield.type], block: Block): TypeInfo =
    doOrYield match {
      case Left(Do) =>
        // TODO: add a way to reference all the created variables in the for statements
        TypeInfo.unit
      case Right(Yield) =>
        // TODO: same as above
        check(block)
    }

  private def checkAskExpr(target: Expression, message: Expression): Expression = {
    /* TODO: Check symbol table to see if the message exists for the given target,
     * and if so, what type does it return? */
    message
  }

  private def checkAskExpr(targets: List[Expression], messages: List[Expression]): TypeInfo = {
    val types = for {
      target  <- targets
      message <- messages
    } yield checkAskExpr(target, message)

    val ref = types.head.nodeType
    val throwTypeMismatches = types.throwIfMismatch(msg => (ref |!=| msg.nodeType) || (ref |!?=| msg.nodeType))(_)
    throwTypeMismatches(msg => s"the message ${lineRef(msg)} does not return type $ref")
    ref
  }

  private def checkFuncall(id: String, args: List[Expression]): TypeInfo = {
    // TODO: lookup function id in symbol table, and get its type
    // TODO: make sure the function arguments match the inputs as dictated by the symbol table
    new TypeInfo("Placeholder")
  }

  private def checkMethodCall(objId: Identifier, funId: Identifier, args: List[Expression]): TypeInfo = {
    // TODO: lookup function id in symbol table, and get its type
    // TODO: make sure the function arguments match the inputs as dictated by the symbol table
    // might actually be a general case of checkFuncall.
    new TypeInfo("Placeholder")
  }

  private def checkFieldCall(objId: Identifier, fieldId: Identifier): TypeInfo = {
    // TODO: lookup function id in symbol table, and get its type
    // TODO: make sure the function arguments match the inputs as dictated by the symbol table
    // might actually be a general case of checkFuncall.
    new TypeInfo("Placeholder")
  }

  private def checkValue(id: Identifier) = {
    // TODO: lookup function id in symbol table, and get its type
    // TODO: make sure the function arguments match the inputs as dictated by the symbol table
    // might actually be a general case of checkFuncall.
    new TypeInfo("Placeholder")
  }

  private def checkBlock(statements: List[Statement]): TypeInfo = {
    statements.foreach(StatementChecker.checkStatement) // evaluate all statements to give them their type
    statements.last.nodeType // last statement in a block is the block's return-type
  }
}