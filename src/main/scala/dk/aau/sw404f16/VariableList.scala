package dk.aau.sw404f16

/**
  * Created by coffee on 5/19/16.
  */
object VariableList {
  val root = new VariableList(null)
}
// uses Vector instead of List due to quicker search time
class VariableList(val parent: VariableList, private var collection: Vector[Either[String, VariableList]] = Vector()) {
  def this(parent: VariableList) = this(parent, Vector())

  def addVariable(variable: String) = collection = Left(variable) +: collection
  def addScope(scope: VariableList) = collection = Right(new VariableList(this)) +: collection

  def currentScope: VariableList = collection.head match {
    case Left(_) => this
    case Right(newScope) => newScope
  }
}
