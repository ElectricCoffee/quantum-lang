package dk.aau.sw404f16.semantics
import dk.aau.sw404f16.semantics.exceptions.{NotYetDeclaredException, VariableExistsException}
import dk.aau.sw404f16.syntax.{ASTNode, Identifier}

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
  // TODO: discuss the potential of using a triple holding (type, node, scope) instead
  private type TableValue = (TypeInfo, Option[ASTNode], Option[SymbolTable])
  /** alias for the symbol table's dictionary type */
  type SymMap = mutable.Map[String, TableValue]

  // private fields
  private val contents: SymMap = mutable.Map.empty

  // private methods, most of which are convenience methods
  /** looks for a value in the symbol table, if not found, it tries the parent scope/table
    * it continues like this until hitting the root scope, after which it throws an NPE
    * @param key The identifier we need to find as a string
    * @return Some if an identifier is found, None if none is found
    */
  private def findValue(key: String): Option[TableValue] = contents get key orElse {
    if (parentScope == null) None
    else parentScope findValue key
  }

  /** looks for a value in the symbol table, if not found, it tries the parent scope/table
    * it continues like this until hitting the root scope, after which it throws an NPE
    * @param key The identifier we need to find as an instance of Identifier
    * @return Some if an identifier is found, None if none is found
    */
  private def findValue(key: Identifier): Option[TableValue] = findValue(key.data)

  /** convenience method to throw a NotYetDeclaredException */
  private def noSuchIdentifier(name: String) =
    NotYetDeclaredException(s"The identifier $name hasn't been declared")

  /** convenience method to create a table-value with no associated scope */
  private def mkValue(typeInfo: TypeInfo, node: ASTNode): TableValue = (typeInfo, Some(node), None)

  /** convenience method to create a table-value with associated scope */
  private def mkValue(typeInfo: TypeInfo, node: ASTNode, scope: SymbolTable): TableValue =
    (typeInfo, Some(node), Some(scope))

  /** convenience method to create a table-value with a scope but no associated value (good for modules) */
  private def mkScope(scope: SymbolTable): TableValue = (TypeInfo.nothing, None, Some(scope))

  private def mkField(typeInfo: TypeInfo): TableValue = (typeInfo, None, None)

  // public methods
  /** adds a new identifier with an ASTNode to the current scope
    * @param id identifier of the ASTNode being added
    * @param node the ASTNode being added
    * @return the new scope if the ASTNode has a body, returns current scope otherwise
    */
  def addIdentifier(typeInfo: TypeInfo, id: String, node: ASTNode): SymbolTable = {
    // see if value exists
    if (contents contains id)
      throw VariableExistsException(s"The variable $id has already been declared in this scope")

    // if it doesn't, check if the ASTNode has a scope, add it and return the new scope
    if (node.hasScope) {
      val newScope = new SymbolTable(this) // the new scope has the current scope as its parent
      val value = mkValue(typeInfo, node, newScope)
      // TODO: also evaluate all the other variables and put them into the new scope
      contents += id -> value
      newScope
    }
    else { // if it doesn't have a scope, add the ASTNode without associating it with a new scope
      contents += id -> mkValue(typeInfo, node) // if it doesn't already exist
      this
    }
  }

  /** adds a new identifier with an ASTNode to the current scope
    * @param id identifier of the ASTNode being added
    * @param node the ASTNode being added
    * @return the new scope if the ASTNode has a body, returns current scope otherwise
    */
  def addIdentifier(typeInfo: TypeInfo, id: Identifier, node: ASTNode): SymbolTable =
    addIdentifier(typeInfo, id.data, node)

  def addIdentifier(id: String, node: ASTNode): SymbolTable = addIdentifier(TypeInfo.undefined, id, node)
  def addIdentifier(id: Identifier, node: ASTNode): SymbolTable = addIdentifier(id.data, node)

  /** adds a new scope with no associated ASTNode to the current scope
    * @param id identifier of the scope being added as a string
    * @param newScope the new scope being added
    * @return the new just-added scope
    */
  def addScope(id: String, newScope: SymbolTable): SymbolTable = {
    contents += id -> mkScope(newScope)
    newScope
  }

  /** adds a new scope with no associated ASTNode to the current scope
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

  def addField(id: String, typeInfo: TypeInfo): SymbolTable = {
    contents += id -> mkField(typeInfo)
    this
  }

  def addField(id: Identifier, typeInfo: TypeInfo): SymbolTable = addField(id.data, typeInfo)

  /** bulk-add of identifiers */
  def addIdentifiers(ids: List[(TypeInfo, String, ASTNode)]) =
    ids.foreach(x => addIdentifier(x._1, x._2, x._3))

//  def addNested(ids: List[Identifier]): SymTable = ids match {
//    case Nil => contents
//    case Identifier(head) :: tail => contents += head -> mkScope(addNested(tail))
//  }

  /** gets the type of an identifier from the symbol table */
  def getType(identifier: String): TypeInfo = getValue(identifier).nodeType

  def getType(identifier: Identifier): TypeInfo = getType(identifier.data)

  /** gets the value associated with an identifier */
  def getValue(identifier: String): ASTNode = findValue(identifier) match {
    case Some((_, Some(v), _)) => v // TODO: may be wrong, look at later
    case None => throw noSuchIdentifier(identifier)
  }

  def getValue(identifier: Identifier): ASTNode = getValue(identifier.data)
}
