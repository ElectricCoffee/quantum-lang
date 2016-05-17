package dk.aau.sw404f16.semantics

import dk.aau.sw404f16.syntax._
import dk.aau.sw404f16.util.{Middle, Top}
import dk.aau.sw404f16.util.Convenience.mkRandomId

import scala.collection._

/**
  * Created by coffee on 10/05/16.
  */
// I have a vague feeling it might be easier to traverse the whole thing after the AST's made
object SymbolTablePopulator {
  private val scopeStack: mutable.Stack[SymbolTable] = mutable.Stack(SymbolTable.root)
  private def currentScope: SymbolTable = scopeStack.head
  private def mkNewScopeAtCurrent(): SymbolTable = {
    val newScope = new SymbolTable(currentScope)
    scopeStack push newScope
    newScope
  }
  private def exitCurrentScope: SymbolTable = scopeStack.pop
  private def doInNewScope(body: SymbolTable => Unit): SymbolTable = { // abstractions galore!
    val newScope = mkNewScopeAtCurrent()
    body(newScope)
    exitCurrentScope
  }
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
        currentScope.addIdentifier(mkRandomId("block"), blck)
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
        val randMatchId = mkRandomId("match")
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
