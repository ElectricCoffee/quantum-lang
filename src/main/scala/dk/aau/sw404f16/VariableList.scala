package dk.aau.sw404f16

/**
  * Created by coffee on 5/19/16.
  */
object VariableList {
  private val scope = scala.collection.mutable.Stack(new Scope(null))

  def currentScope: Scope = scope.head.collection.head match {
    case Right(innerScope) => innerScope
    case _                 => scope.head
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

  def getScope   = scope.head
  def leaveScope = scope.pop
}

class Scope(val parent: Scope, var collection: Vector[Either[String, Scope]] = Vector()) {
  def addValue(name: String): Scope = {
    collection = Left(name) +: collection
    this
  }

  def addScope(newScope: Scope): Scope = {
    collection = Right(newScope) +: collection
    newScope
  }
}
