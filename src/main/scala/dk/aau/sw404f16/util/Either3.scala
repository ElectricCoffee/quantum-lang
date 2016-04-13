package dk.aau.sw404f16.util

/**
  * Created by coffee on 4/13/16.
  */
trait Either3[+A, +B, +C]
case class Top[A](value: A) extends Either3[A, Nothing, Nothing]
case class Middle[B](value: B) extends Either3[Nothing, B, Nothing]
case class Bottom[C](value: C) extends Either3[Nothing, Nothing, C]
