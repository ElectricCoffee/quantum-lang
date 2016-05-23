package dk.aau.sw404f16
import scala.collection.mutable

/**
  * Created by coffee on 5/19/16.
  */
object VariableList {
  private val scope = scala.collection.mutable.Stack(new Scope(null))

  def currentScope: Scope = scope.head.collection.head match {
    case (Right(innerScope), _) => innerScope
    case _                      => scope.head
  }

  def contains(name: String): Boolean = currentScope.collection contains Left(name)

  def addValue(name: String): Scope = {
    if (contains(name))
      throw new IllegalArgumentException(s"the value $name already exists in current scope")
    currentScope addValue name
  }

  def addScope(): Scope = {
    val newScope = new Scope(currentScope)
    currentScope addScope newScope
    scope push newScope
    currentScope
  }

  def hasAssociatedRecord(name: String) = {
    if (!contains(name))
      throw new IllegalArgumentException(s"the value $name hasn't been defined")

    currentScope.collection(Left(name)).isDefined // returns true if there is an associated record
  }

  def getAssociatedRecord(name: String): String = {
    if (hasAssociatedRecord(name))
      currentScope.collection(Left(name)).get
    else
      throw new IllegalArgumentException(s"the value $name doesn't have an associated record")
  }

  def getAssociatedRecord(name: String, default: String): String = try {
    getAssociatedRecord(name)
  } catch {
    case iae: IllegalArgumentException => default
  }

  def getScope   = scope.head
  def leaveScope = scope.pop
}

class Scope(val parent: Scope, val collection: mutable.Map[Either[String, Scope], Option[String]] = mutable.Map.empty) {
  def addValue(name: String): Scope = {
    collection += Left(name) -> None
    this
  }

  def addValue(name: String, struct: String): Scope = {
    collection += Left(name) -> Some(struct)
    this
  }

  def addScope(newScope: Scope): Scope = {
    collection += Right(newScope) -> None
    newScope
  }
}
