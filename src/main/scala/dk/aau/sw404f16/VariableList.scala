package dk.aau.sw404f16

/**
  * Created by coffee on 5/19/16.
  */
object VariableList {
  val root = new VariableList(null)
}

class VariableList(val parent: VariableList, private var collection: List[Either[String, VariableList]] = Nil) {
  def this(parent: VariableList) = this(parent, Nil)

  def addVariable(variable: String) = collection = Left(variable) :: collection
  def addScope(scope: VariableList) = collection = Right(new VariableList(this)) :: collection

  def currentScope: VariableList = collection.head match {
    case Left(_) => this
    case Right(newScope) => newScope
  }
}
