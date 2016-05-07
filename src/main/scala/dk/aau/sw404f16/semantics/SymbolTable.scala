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
  private type TableValue = (Expression, Option[SymbolTable])
  type SymTable = mutable.Map[String, TableValue]

  // private fields
  private val contents: SymTable = mutable.Map.empty

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
  private def mkValue(expr: Expression): TableValue = (expr, None)
  private def mkValue(expr: Expression, scope: SymbolTable): TableValue = (expr, Some(scope))

  // public methods
  def addIdentifier(id: String, expr: Expression) = {
    if(expr.hasScope) {
      val value = mkValue(expr, new SymbolTable(this))
      // TODO: also evaluate all the other variables and put them into the new scope
      contents += id -> value
    }
    else {
      // see if value exists
      if (contents contains id) throw VariableExistsException(s"The variable $id has already been declared")

      contents += id -> mkValue(expr) // if it doesn't already exist
    }
  }

  def addIdentifier(id: Identifier, expr: Expression) = addIdentifier(id.data, expr)

  def addIdentifiers(ids: List[(String, Expression)]) = ids.foreach(x => addIdentifier(x._1, x._2))

  /** gets the type of an identifier from the symbol table */
  def getType(identifier: String): TypeInfo = getValue(identifier).nodeType

  def getType(identifier: Identifier): TypeInfo = getType(identifier.data)

  def getValue(identifier: String): Expression = findValue(identifier) match {
    case Success(v) => v._1
    case Failure(_) => throw noSuchIdentifier(identifier)
  }

  def getValue(identifier: Identifier): Expression = getValue(identifier.data)
}
