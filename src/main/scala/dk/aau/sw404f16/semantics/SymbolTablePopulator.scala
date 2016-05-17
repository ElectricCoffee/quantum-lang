package dk.aau.sw404f16.semantics

import dk.aau.sw404f16.syntax._
import dk.aau.sw404f16.util.{Middle, Top}
import dk.aau.sw404f16.util.Convenience.{mkNamedUID, mkUUID}

import scala.collection._

/**
  * Created by coffee on 10/05/16.
  */
// I have a vague feeling it might be easier to traverse the whole thing after the AST's made
object SymbolTablePopulator {
  /** A stack representing the scope hierarchy,
    * it's there to make it easy to refer back to the parent scope without fiddling with references
    */
  private val scopeStack: mutable.Stack[SymbolTable] = mutable.Stack(SymbolTable.root)

  /** Convenience method that finds and returns the current scope */
  private def currentScope: SymbolTable = scopeStack.head

  private def mkNewScope(id: String, scope: SymbolTable): SymbolTable = { // "mk" is short for "make"
    val newScope = scope.addScope(id)
    scopeStack push newScope
    newScope
  }

  /** Creates a new sub-scope based on the current, and pushes it onto the stack before returning it */
  private def mkNewScopeAtCurrent(id: String): SymbolTable = mkNewScope(id, currentScope)

  private def mkNewScopeAtCurrent(): SymbolTable = mkNewScopeAtCurrent(mkUUID)

  /** Removes the current scope from the stack and returns it */
  private def exitCurrentScope: SymbolTable = scopeStack.pop

  /** Abstraction of the pattern of making a new scope, doing stuff, then exiting it */
  private def doInNewScope(id: String)(body: SymbolTable => Unit): SymbolTable = { // abstractions galore!
  val newScope = mkNewScopeAtCurrent(id)
    body(newScope)
    exitCurrentScope
  }

  private def doInNewScope(body: SymbolTable => Unit): SymbolTable = doInNewScope(mkUUID)(body)

  /** Similar to doInNewScope except it uses an existing scope instead of making a new one */
  private def doInScope(init: => SymbolTable)(body: SymbolTable => Unit): SymbolTable = {
    scopeStack push init
    body(currentScope)
    exitCurrentScope
  }

  def populateWithProgram(program: Program) = {
    val Program(ModuleName(module), constructors) = program
    val moduleName = module.mkString(".")
    scopeStack push (mkNewScopeAtCurrent() addScope moduleName)
    constructors.foreach(populateWithTopLevel)
    // TBD
  }

  def populateWithTopLevel(tlc: TopLevelCons) = tlc match {
    case ModuleImport(ModuleName(mn)) => ???
    case ActorDefinition(pType, inhType, ActorBodyBlock(block))    =>
      populateWithActor('actor, pType, inhType, block)
    case ReceiverDefinition(pType, inhType, ActorBodyBlock(block)) =>
      populateWithActor('receiver, pType, inhType, block)
    case DataStructureDefinition(typeDef, types, block) => ???
  }

  def populateWithActor(kind: Symbol, primary: TypeDefinition, optionalSuper: Option[List[TypeDefinition]], body: List[MessageDefinition]) = {
    val superTypes = optionalSuper.toList.flatten.map(_.toTypeInfo)
    val primaryType = primary.toTypeInfo makeSubtypeOf superTypes
    // TBD
  }

  def populateWithMessageDef(body: MessageDefinition) = body match {
    case MessageDefinition(typeDef, PatternDefinition(pattern), Block(block)) =>
      val returnType = typeDef.toTypeInfo
      populateWithBlock(block)
      // TBD
  }

  def populateWithBlock(body: List[Statement]): SymbolTable = doInNewScope { ns =>
    def populateIfScoped(expr: Expression) =
      if (expr.hasScope) populateWithScopedExpression(expr)
    // TODO: figure out a decent way to do this
    body.map(_.stmt).foreach {
      case Top(expr) =>
        populateIfScoped(expr)

      case Middle(ValueDefinition(Left(id), expr)) =>
        ns.addIdentifier(id, expr)
        populateIfScoped(expr)

      case Middle(ValueDefinition(Right(TypedValue(typeDef, id)), expr)) =>
        ns.addIdentifier(typeDef.toTypeInfo, id, expr)
        populateIfScoped(expr)
    }
  }

  def populateWithScopedExpression(expr: Expression): SymbolTable = {
    assert(expr.hasScope) // throws an exception if expr.hasScope is false
    expr match {
      // TODO: figure out a decent way to do this. Scrap this later maybe, seems redundant to add a scope each time
      case blck@Block(data) =>
        currentScope.addIdentifier(mkNamedUID("block"), blck)
        populateWithBlock(data)
      case ifExpr@IfExpression(stmts) =>
        val randIfId = mkRandomId("if")
        doInScope(currentScope.addIdentifier(randIfId, ifExpr)) { s =>
          for (i <- stmts.indices) {
            s.addIdentifier(s"if-expr-${i + 1}@$randIfId", stmts(i))
            // TODO: each statement within an if-expression is its own scope, find a way to do this
          }
        }
      case matchExpr@MatchExpression(_, stmts) =>
        val randMatchId = mkNamedUID("match")
        doInScope(currentScope.addIdentifier(randMatchId, matchExpr)) { s =>
          for (i <- stmts.indices) {
            s.addIdentifier(s"match-expr-${i + 1}@$randMatchId", stmts(i))
            // TODO: each statement within a match-expression is its own scope, find a way to do this
          }
        }
      case forCompr@ForComprehension(stmts, doOrYield, block) =>
        // TODO: the entire for-comprehension is one coherent block, which differs from the other control structures
        ???
      case other => currentScope // if any other input, do nothing and return the current scope
    }
  }
}
