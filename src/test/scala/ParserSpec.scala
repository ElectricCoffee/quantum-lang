import org.scalatest._
import scala.util.parsing.combinator._
import dk.aau.sw404f16.syntax.parser.{Parser => NParser}
import dk.aau.sw404f16.syntax.{BinaryOperation, Identifier, IfExpression, IfStatement, Operator, Statement, StringLiteral}
import dk.aau.sw404f16.util.Bottom
import dk.aau.sw404f16.syntax.NumberLiteral

/**
  * Created by coffee on 3/30/16.
  * Documentation for ScalaTest available on http://www.scalatest.org/
  */
class ParserSpec extends FlatSpec with Matchers with RegexParsers {
  "The Parser" should "parse a string as an operator" in {
    val inputs = List("<", ">", "==", ">>=", "<*>", "<|>", "->")
    val expected = List(Operator("<"), Operator(">"), Operator("=="), Operator(">>="), Operator("<*>"), Operator("<|>"), Operator("->"))

    val result = inputs.map(input => NParser.parse(NParser.operator, input))
    result should be (expected)
  }

  it should "parse a string as a valid stringliteral" in {
    val input = "\"hej med dig\""
    val expectation = StringLiteral(input)

    val result = NParser.parse(NParser.stringLiteral, input)
    result should be (expectation)
  }

  it should "parse a string as a valid numberliteral" in {
    val input = "1234"
    val expectation = NumberLiteral("1234")

    val result = NParser.parse(NParser.numberLiteral, input)
    result should be (expectation)
  }

  it should "parse a string as a valid function call" in {
    import dk.aau.sw404f16.syntax.FunctionCall
    val input = "add(2, b)"
    val expectation = FunctionCall(Identifier("add"), List(NumberLiteral("2"), Identifier("b")))
    val result = NParser.parse(NParser.funCall, input)
    result should be (expectation)
  }

  it should "parse a string as a valid method call" in {
    import dk.aau.sw404f16.syntax.{FunctionCall, MethodCall}
    val input = "math.pow(3, 4)"
    val expectation = MethodCall(Identifier("math"), FunctionCall(Identifier("pow"), List(NumberLiteral("3"), NumberLiteral("4"))))
    val result = NParser.parse(NParser.methodCall, input)
    result should be (expectation)
  }

  it should "parse a string as a valid field call" in {
    import dk.aau.sw404f16.syntax.FieldCall
    val input = "math.pi"
    val expectation = FieldCall(Identifier("math"), Identifier("pi"))
    val result = NParser.parse(NParser.fieldCall, input)
    result should be (expectation)
  }
  it should "Parse a string as a valid atom with" in {
    import dk.aau.sw404f16.syntax.{Atom, AtomConstruct}
    val input1 = "#pop"
    val input2 = "#push(12)"
    val expectation1 = AtomConstruct(Atom("#pop"), None)
    val expectation2 = AtomConstruct(Atom("#push"), Some(List(NumberLiteral("12"))))
    val result1 = NParser.parse(NParser.atom, input1)
    val result2 = NParser.parse(NParser.atom, input2)
    result1 should be (expectation1)
    result2 should be (expectation2)
  }
  it should "parse a string as a valid module name" in {
    import dk.aau.sw404f16.syntax.{ModuleImport, ModuleName}
    val input = "import namespace.foo.bar.Class;"
    val expectation = ModuleImport(ModuleName(List(Identifier("namespace"), Identifier("foo"), Identifier("bar"), Identifier("Class"))))
    val result = NParser.parse(NParser.moduleImport, input)
    result should be (expectation)
  }

  it should "parse a string as a valid TopLevelCons" in {
    val input = ""
    val expectation = ""
  }

  it should "parse a string as a valid actor definition" in {
    import dk.aau.sw404f16.syntax.{ActorBodyBlock, ActorDefinition, ReceiverDefinition, TypeDefinition}
    val input1 = "actor Program {}"
    val input2 = "receiver Program {}"
    val expectation1 = ActorDefinition(TypeDefinition(Identifier("Program"), None), None, ActorBodyBlock(Nil))
    val expectation2 = ReceiverDefinition(TypeDefinition(Identifier("Program"), None), None, ActorBodyBlock(Nil))
    val result1 = NParser.parse(NParser.actorDef, input1)
    val result2 = NParser.parse(NParser.actorDef, input2)
    result1 should be (expectation1)
    result2 should be (expectation2)
  }
  it should "parse a string as a valid typeParam" in {
    import dk.aau.sw404f16.syntax.{TypeDefinition, TypeParameter}
    val input = "List of String"
    val expectation = TypeDefinition(Identifier("List"), Some(List(TypeParameter(Right(Identifier("String"))))))
    val result = NParser.parse(NParser.typeParam, input)
    result should be (expectation)
  }


  it should "Parse a string as a valid if statement" in {
    val input =
      """
        |if {
        |  a > b then "hia";
        |  c < d then "hello";
        |  p == q then "hi";
        |}
      """.stripMargin
    val case1 = IfStatement(Statement(Bottom(BinaryOperation(Identifier("a"), Operator(">"), Identifier("b")))), StringLiteral("\"hia\""))
    val case2 = IfStatement(Statement(Bottom(BinaryOperation(Identifier("c"), Operator("<"), Identifier("d")))), StringLiteral("\"hello\""))
    val case3 = IfStatement(Statement(Bottom(BinaryOperation(Identifier("p"), Operator("=="), Identifier("q")))), StringLiteral("\"hi\""))
    
    val expectation = IfExpression(List(case1, case2, case3))
    val result = NParser.parse(NParser.ifExpr, input)
    result should be (expectation)
  }
}