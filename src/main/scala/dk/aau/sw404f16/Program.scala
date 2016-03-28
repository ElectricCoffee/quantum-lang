package dk.aau.sw404f16

import dk.aau.sw404f16.lexer.Lexer

/**
  * Created by coffee on 28/03/16.
  */
object Program {
  def main(args: Array[String]) = {
    val lst = List("for", "if", "then", "hello", "=", "actor", "(", "{", "[", "0xbadf00d", ">>=",
      "0b1101111010101101", "#pop-dat-booty", "\"welcome to \\\"Homestarrunner.com\\\"\"")

    lst.map(Lexer.tokenizeString).foreach(println)
  }
}
