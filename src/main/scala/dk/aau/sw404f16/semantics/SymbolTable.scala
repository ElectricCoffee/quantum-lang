package dk.aau.sw404f16.semantics
import dk.aau.sw404f16.semantics.exceptions.{NotYetDeclaredException, VariableExistsException}
import dk.aau.sw404f16.syntax.{BlockLike, Expression, Identifier, NumberLiteral}

import scala.util.{Failure, Success, Try}
import scala.collection.mutable
/**
  * Created by coffee on 4/14/16.
  */
object SymbolTable {
  val root = new SymbolTable(null) // the root scope is special and should always be accessible
}

// writing val/var in front of a constructor parameter makes it public
class SymbolTable(val parentScope: SymbolTable) {
  // type aliases
  private type TableValue = (Option[Expression], Option[SymbolTable])
  type SymMap = mutable.Map[String, TableValue]

  // private fields
  private val contents: SymMap = mutable.Map.empty

  // public fields
//  lazy val scopeName: String = {
//    val parentName = if (parentScope == null) "Root" else parentScope.scopeName
//
//  }

  // private methods
  private def findValue(key: String): Try[TableValue] = Try {
    if (contents contains key) contents(key)
    else parentScope.findValue(key)
  }.flatten

  private def findValue(key: Identifier): Try[TableValue] = findValue(key.data)
  private def noSuchIdentifier(name: String) =
    NotYetDeclaredException(s"The identifier $name hasn't been declared")
  private def mkValue(expr: Expression): TableValue = (Some(expr), None)
  private def mkValue(expr: Expression, scope: SymbolTable): TableValue = (Some(expr), Some(scope))
  private def mkScope(scope: SymbolTable): TableValue = (None, Some(scope))

  // public methods
  def addIdentifier(id: String, expr: Expression) = {
  def addIdentifier(id: String, expr: Expression): SymbolTable = {
    // see if value exists
    if (contents contains id)
      throw VariableExistsException(s"The variable $id has already been declared in this scope")

    // if it doesn't, check if the expression has a scope, add it and return the new scope
    if (expr.hasScope) {
      val newScope = new SymbolTable(this) // the new scope has the current scope as its parent
      val value = mkValue(expr, newScope)
      // TODO: also evaluate all the other variables and put them into the new scope
      contents += id -> value
      newScope
    }
    else { // if it doesn't have a scope, add the expression without associating it with a new scope
      contents += id -> mkValue(expr) // if it doesn't already exist
      this
    }
  }

  def addIdentifier(id: Identifier, expr: Expression): SymbolTable = addIdentifier(id.data, expr)

  def addScope(id: String, newScope: SymbolTable): SymbolTable = {
    contents += id -> mkScope(newScope)
    newScope
  }

  def addScope(id: Identifier, scope: SymbolTable): SymbolTable = addScope(id.data, scope)

  def addScope(id: String): SymbolTable = addScope(id, new SymbolTable(this))

  def addScope(id: Identifier): SymbolTable = addScope(id, new SymbolTable(this))

  def addIdentifiers(ids: List[(String, Expression)]) = ids.foreach(x => addIdentifier(x._1, x._2))

//  def addNested(ids: List[Identifier]): SymTable = ids match {
//    case Nil => contents
//    case Identifier(head) :: tail => contents += head -> mkScope(addNested(tail))
//  }

  /** gets the type of an identifier from the symbol table */
  def getType(identifier: String): TypeInfo = getValue(identifier).nodeType

  def getType(identifier: Identifier): TypeInfo = getType(identifier.data)

  def getValue(identifier: String): Expression = findValue(identifier) match {
    case Success((Some(v), _)) => v
    case _ => throw noSuchIdentifier(identifier)
  }

  def getValue(identifier: Identifier): Expression = getValue(identifier.data)
}
