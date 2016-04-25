package dk.aau.sw404f16.util

import scala.io.{Source, BufferedSource}

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

  implicit class RichStringList(val self: List[String]) extends AnyVal {
    def stripSpaces: List[String] = self.filter {
      case whitespace() | "" => false // exclude whitespace and empty strings
      case _                 => true  // include everything else
    }
  }

  implicit class RichTupleList[+A, +B](val self: List[(A, B)]) extends AnyVal {
    /** inverts the relation, making it go from List[(A, B)] to (List[A], List[B]) */
    def toTuple: (List[A], List[B]) = {
      val lhs = self.map(_._1)
      val rhs = self.map(_._2)
      (lhs, rhs)
    }
  }
}
