package dk.aau.sw404f16

import dk.aau.sw404f16.syntax.parser.Parser

import scala.io.Source

/**
  * Created by coffee on 28/03/16.
  */
object Program {
  def main(args: Array[String]) = {
    println("It compiled!")
    // look in src/test/scala for the tests
    val lines =
      """module test;
        |import Foo;
        |receiver Program {
        |  define Unit #main = {
        |    val Num a = 3;
        |    val b = "hello world";
        |
        |    if a > b then "hia";
        |
        |    for i in Bar do "hello";
        |
        |    for {
        |     i in K;
        |     j in F;
        |    } yield {foo;};
        |
        |    match (foo) (Num bar) then stuff;
        |    match (bar) {
        |      "stuff" then stuff;
        |      (Num baz) then morestuff;
        |    };
        |  };
        |  define string #foo(bar) = "hello";
        |  define num (string quux) = quux;
        |}
      """.stripMargin

    println(Parser.apply(lines))
  }
}
