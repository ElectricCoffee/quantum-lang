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
  /** alias for a tuple of two options */
  // TODO: discuss the potential of using a triple holding (type, expr, scope) instead
  private type TableValue = (TypeInfo, Option[Expression], Option[SymbolTable])
  /** alias for the symbol table's dictionary type */
  type SymMap = mutable.Map[String, TableValue]

  // private fields
  private val contents: SymMap = mutable.Map.empty

  // public fields
//  lazy val scopeName: String = {
//    val parentName = if (parentScope == null) "Root" else parentScope.scopeName
//
//  }

  // private methods, most of which are convenience methods
  /** looks for a value in the symbol table, if not found, it tries the parent scope/table
    * it continues like this until hitting the root scope, after which it throws an NPE
    * @param key The identifier we need to find as a string
    * @return Success if an identifier is found, Failure if none is found
    */
  private def findValue(key: String): Try[TableValue] = Try {
    if (contents contains key) Try(contents(key))
    else parentScope findValue key
  }.flatten // needs to be flattened, because otherwise the type would be Try[Try[Try[Try[... Try[TableValue]...]]]]

  /** looks for a value in the symbol table, if not found, it tries the parent scope/table
    * it continues like this until hitting the root scope, after which it throws an NPE
    * @param key The identifier we need to find as an instance of Identifier
    * @return Success if an identifier is found, Failure if none is found
    */
  private def findValue(key: Identifier): Try[TableValue] = findValue(key.data)

  /** convenience method to throw a NotYetDeclaredException */
  private def noSuchIdentifier(name: String) =
    NotYetDeclaredException(s"The identifier $name hasn't been declared")

  /** convenience method to create a table-value with no associated scope */
  private def mkValue(typeInfo: TypeInfo, expr: Expression): TableValue = (typeInfo, Some(expr), None)

  /** convenience method to create a table-value with associated scope */
  private def mkValue(typeInfo: TypeInfo, expr: Expression, scope: SymbolTable): TableValue =
    (typeInfo, Some(expr), Some(scope))

  /** convenience method to create a table-value with a scope but no associated value (good for modules) */
  private def mkScope(scope: SymbolTable): TableValue = (TypeInfo.nothing, None, Some(scope))

  private def mkField(typeInfo: TypeInfo): TableValue = (typeInfo, None, None)

  // public methods
  /** adds a new identifier with an expression to the current scope
    * @param id identifier of the expression being added
    * @param expr the expression being added
    * @return the new scope if the expression has a body, returns current scope otherwise
    */
  def addIdentifier(typeInfo: TypeInfo, id: String, expr: Expression): SymbolTable = {
    // see if value exists
    if (contents contains id)
      throw VariableExistsException(s"The variable $id has already been declared in this scope")

    // if it doesn't, check if the expression has a scope, add it and return the new scope
    if (expr.hasScope) {
      val newScope = new SymbolTable(this) // the new scope has the current scope as its parent
      val value = mkValue(typeInfo, expr, newScope)
      // TODO: also evaluate all the other variables and put them into the new scope
      contents += id -> value
      newScope
    }
    else { // if it doesn't have a scope, add the expression without associating it with a new scope
      contents += id -> mkValue(typeInfo, expr) // if it doesn't already exist
      this
    }
  }

  /** adds a new identifier with an expression to the current scope
    * @param id identifier of the expression being added
    * @param expr the expression being added
    * @return the new scope if the expression has a body, returns current scope otherwise
    */
  def addIdentifier(typeInfo: TypeInfo, id: Identifier, expr: Expression): SymbolTable =
    addIdentifier(typeInfo, id.data, expr)

  /** adds a new scope with no associated expression to the current scope
    * @param id identifier of the scope being added as a string
    * @param newScope the new scope being added
    * @return the new just-added scope
    */
  def addScope(id: String, newScope: SymbolTable): SymbolTable = {
    contents += id -> mkScope(newScope)
    newScope
  }

  /** adds a new scope with no associated expression to the current scope
    * @param id identifier of the scope being added
    * @param newScope the new scope being added
    * @return the new just-added scope
    */
  def addScope(id: Identifier, newScope: SymbolTable): SymbolTable = addScope(id.data, newScope)

  /** adds a new scope  that is the child of the current scope
    * @param id the identifier of the scope being added as a string
    * @return the new just-added scope
    */
  def addScope(id: String): SymbolTable = addScope(id, new SymbolTable(this))

  /** adds a new scope  that is the child of the current scope
    * @param id the identifier of the scope being added
    * @return the new just-added scope
    */
  def addScope(id: Identifier): SymbolTable = addScope(id, new SymbolTable(this))

  /** bulk-add of identifiers */
  def addIdentifiers(ids: List[(TypeInfo, String, Expression)]) =
    ids.foreach(x => addIdentifier(x._1, x._2, x._3))

//  def addNested(ids: List[Identifier]): SymTable = ids match {
//    case Nil => contents
//    case Identifier(head) :: tail => contents += head -> mkScope(addNested(tail))
//  }

  /** gets the type of an identifier from the symbol table */
  def getType(identifier: String): TypeInfo = getValue(identifier).nodeType

  def getType(identifier: Identifier): TypeInfo = getType(identifier.data)

  /** gets the value associated with an identifier */
  def getValue(identifier: String): Expression = findValue(identifier) match {
    case Success((_, Some(v), _)) => v // TODO: may be wrong, look at later
    case _ => throw noSuchIdentifier(identifier)
  }

  def getValue(identifier: Identifier): Expression = getValue(identifier.data)
}
