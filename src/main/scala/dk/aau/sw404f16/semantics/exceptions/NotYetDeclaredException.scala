package dk.aau.sw404f16.semantics.exceptions

/**
  * Created by coffee on 4/21/16.
  */
case class NotYetDeclaredException(message: String = null, cause: Throwable = null)
  extends Exception(message, cause)
