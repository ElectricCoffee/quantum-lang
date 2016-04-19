package dk.aau.sw404f16.semantics

/**
  * Created by coffee on 4/19/16.
  */
object StandardType {
  private def typeWithNoArgs(t: String) = (t, Nil)
  val number = typeWithNoArgs("number")
  val string = typeWithNoArgs("string")
  val boolean = typeWithNoArgs("boolean")
}
