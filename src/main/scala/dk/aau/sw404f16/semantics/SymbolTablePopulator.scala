package dk.aau.sw404f16.semantics

import dk.aau.sw404f16.syntax._
import dk.aau.sw404f16.util.{Middle, Top}

import scala.collection._

/**
  * Created by coffee on 10/05/16.
  */
// I have a vague feeling it might be easier to traverse the whole thing after the AST's made
object SymbolTablePopulator {
  private val scope: mutable.Stack[SymbolTable] = mutable.Stack(SymbolTable.root)
  private def currentScope: SymbolTable = scope.head // gets the topmost value of the stack without popping it
  private def mkNewScopeAtCurrent(): SymbolTable = {
    val newScope = new SymbolTable(currentScope)
    scope push newScope
    newScope
  }
  private def exitCurrentScope: SymbolTable = scope.pop
  private def doInNewScope(body: SymbolTable => Unit): SymbolTable = { // abstractions galore!
    val newScope = mkNewScopeAtCurrent()
    body(newScope)
    exitCurrentScope
  }

  def populateWithProgram(program: Program) = {
    val Program(ModuleName(module), constructors) = program
    val moduleName = module.mkString(".")
    scope push (mkNewScopeAtCurrent() addScope moduleName)
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
    val newScope = mkNewScopeAtCurrent()
    // TBD
  }

  def populateWithMessageDef(body: MessageDefinition) = body match {
    case MessageDefinition(typeDef, PatternDefinition(pattern), block) =>
      val returnType = typeDef.toTypeInfo
  }

  def populateWithBlock(body: List[Statement]): SymbolTable = doInNewScope { newScope =>
    body.map(_.stmt).foreach {
      case Top(expr) => populateWithExpression(expr)
      case Middle(ValueDefinition(Left(id), expr)) =>
        scope push newScope.addIdentifier(id, expr)
        populateWithExpression(expr)
      case Middle(ValueDefinition(Right(TypedValue(typeDef, id)), expr)) =>
        scope push newScope.addIdentifier(typeDef.toTypeInfo, id, expr)
        populateWithExpression(expr)
    }
  }

  def populateWithExpression(expr: Expression): TypeInfo = ???
}
