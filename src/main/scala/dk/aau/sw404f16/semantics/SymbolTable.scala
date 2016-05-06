package dk.aau.sw404f16.semantics
import dk.aau.sw404f16.semantics.exceptions.NotYetDeclaredException
import dk.aau.sw404f16.syntax.{BlockLike, Expression, Identifier}

import scala.util.{Failure, Success, Try}
import scala.collection.mutable
/**
  * Created by coffee on 4/14/16.
  */
object SymbolTable {
  val global = new SymbolTable("Global") // the global scope is special and should always be accessible?
}

class SymbolTable(currentScope: String) {
  // type aliases
  private type TableValue = (Expression, Option[SymbolTable])
  type SymTable = mutable.Map[String, TableValue]

  // private fields
  private val contents: SymTable = mutable.Map.empty

  // private methods
  private def findValue(key: String): Try[TableValue] = Try(contents(key))
  private def findValue(key: Identifier): Try[TableValue] = findValue(key.data)
  private def noSuchIdentifier(name: String) =
    NotYetDeclaredException(s"The identifier $name hasn't been declared")
  private def mkValue(expr: Expression): TableValue = (expr, None)
  private def mkValue(expr: Expression, scope: SymbolTable): TableValue = (expr, Some(scope))

  // public methods
  /** gets the type of an identifier from the symbol table */
  def getType(identifier: String): TypeInfo = findValue(identifier) match {
    case Success(v) => v._1.nodeType
    case Failure(_) => throw noSuchIdentifier(identifier)
  }

  def getType(identifier: Identifier): TypeInfo = getType(identifier.data)

  def getValue(identifier: String): Expression = findValue(identifier) match {
    case Success(v) => ???
    case Failure(_) => throw noSuchIdentifier(identifier)
  }

  def getValue(identifier: Identifier): Expression = getValue(identifier.data)
}
