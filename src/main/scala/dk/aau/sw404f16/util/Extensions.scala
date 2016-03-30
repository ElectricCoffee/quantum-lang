package dk.aau.sw404f16.util

import scala.annotation.tailrec

/**
  * Created by coffee on 3/30/16.
  * val head :: tail = input
  * if(tail != Nil) head match {
  * case " " | "" => stripSpaces(tail, acc)
  * case _   => stripSpaces(tail, head :: acc)
  * } else acc
  */
object Extensions {
  type StrLst = List[String] // type defines an alias
  // implicit classes are how you add extension methods to classes
  // they must exist within an object
  implicit class RichStringList(val left: StrLst) extends AnyVal {
    def stripSpaces: StrLst = left.filter {
      case " " | "" => false
      case _        => true
    }
  }
}
