package dk.aau.sw404f16.util

/** Created by coffee on 3/30/16.
  * This file is for extension methods
  * If you need to extend an existing class,
  * create an "implicit class" in the Extensions object
  * You can read more about implicit classes here:
  * http://docs.scala-lang.org/overviews/core/implicit-classes.html
  */
object Extensions {
  private val whitespace = "\\s".r // matches all whitespace, including spaces, tabs, and newlines
  implicit class RichStringList(val left: List[String]) extends AnyVal {
    def stripSpaces: List[String] = left.filter {
      case whitespace() | "" => false // exclude whitespace and empty strings
      case _                 => true  // include everything else
    }
  }
}
