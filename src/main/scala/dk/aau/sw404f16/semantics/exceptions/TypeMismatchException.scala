package dk.aau.sw404f16.semantics.exceptions

/**
  * Created by coffee on 4/19/16.
  */
case class TypeMismatchException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
