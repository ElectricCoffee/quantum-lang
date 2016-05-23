package dk.aau.sw404f16.util

import dk.aau.sw404f16.syntax.ASTNode

import scala.io.{BufferedSource, Source}

/** Created by coffee on 3/30/16.
  * This file is for extension methods
  * If you need to extend an existing class,
  * create an "implicit class" in the Extensions object
  * You can read more about implicit classes here:
  * http://docs.scala-lang.org/overviews/core/implicit-classes.html
  */
object Extensions {
  private val whitespace = "\\s".r // matches all whitespace, including spaces, tabs, and newlines
  implicit class RichString(val left: String) extends AnyVal {
    def getFileContents: BufferedSource = Source.fromFile(getClass.getClassLoader.getResource(left).getPath)
  }

  implicit class RichStringList(val left: List[String]) extends AnyVal {
    def stripSpaces: List[String] = left.filter {
      case whitespace() | "" => false // exclude whitespace and empty strings
      case _                 => true  // include everything else
    }
  }

  implicit class RichNodeList(val left: List[ASTNode]) extends AnyVal {
    /** abstracts away a very common pattern in code-generation */
    def mkElixir: List[String] = left.map(_.toElixir)

    /** creates a string consisting of elixir expressions separated by the separator */
    def mkElixirString(separator: String): String = mkElixir.mkString(separator)

    /** creates a string of elixir expressions separated by comma */
    def mkElixirString: String = mkElixirString(", ")
  }
}
