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
  // tail-recursive methods are optimised into loops by the compiler,
  // and thus are more efficient
  @tailrec private def stripSpaces(input: List[String], acc: List[String]): List[String] = input match {
    case Nil          => acc
    case head :: tail => head match {
      case " " | "" => stripSpaces(tail, acc)
      case _        => stripSpaces(tail, head :: acc)
    }
  }
  // implicit classes are how you add extension methods to classes
  // they must exist within an object
  implicit class RichStringList(val left: List[String]) extends AnyVal {
    def stripSpaces: List[String] = Extensions.stripSpaces(left, Nil).reverse
  }
}
