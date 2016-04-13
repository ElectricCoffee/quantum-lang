package dk.aau.sw404f16

import dk.aau.sw404f16.syntax.parser.Parser

import dk.aau.sw404f16.util.Extensions.RichString

/**
  * Created by coffee on 28/03/16.
  */
object Program {
  def main(args: Array[String]): Unit = {
    println("It compiled!")
    // look in src/test/scala for the tests
    val lines = "test.q".getFileContents.mkString

    println(Parser apply lines)
  }
}
