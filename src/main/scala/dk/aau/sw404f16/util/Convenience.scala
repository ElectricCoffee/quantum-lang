package dk.aau.sw404f16.util

/**
  * Created by coffee on 4/23/16.
  */
object Convenience {
  /** for when code paths that are impossible to reach,
    * but the compiler won't let you continue without adding the last catch-all case
    */
  def !!! = throw new UnsupportedOperationException("This code shouldn't be reachable")
}
