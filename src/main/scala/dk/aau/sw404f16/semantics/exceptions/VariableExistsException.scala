package dk.aau.sw404f16.semantics.exceptions

/**
  * Created by coffee on 5/6/16.
  */
case class VariableExistsException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
