package dk.aau.sw404f16.semantics
import dk.aau.sw404f16.semantics.exceptions.NotYetDeclaredException
import dk.aau.sw404f16.syntax.{ASTNode, Identifier}

import scala.util.{Failure, Success, Try}
import scala.collection.mutable
/**
  * Created by coffee on 4/14/16.
  */
class SymbolTable(scope: String) {
  type TypeInfo = Option[(String, List[String])]
  type SymTab = mutable.Map[String, Either[(TypeInfo, ASTNode), SymbolTable]]
  var contents: SymTab = mutable.Map.empty

  private def findValue(key: Identifier): Try[Either[(TypeInfo, ASTNode), SymbolTable]] = Try(contents(key.data))
  private def noSuchIdentifier(name: String) =
    NotYetDeclaredException("The identifier "+ name +" hasn't been declared")

  /** gets the type of an identifier from the symbol table */
  def getType(identifier: Identifier): TypeInfo = findValue(identifier) match {
    case Success(v) => v.left.get._1
    case Failure(_) => throw noSuchIdentifier(identifier.data)
  }

  def getValue(identifier: Identifier): ASTNode = findValue(identifier) match {
    case Success(v) => ???
    case Failure(_) => throw noSuchIdentifier(identifier.data)
  }
}
