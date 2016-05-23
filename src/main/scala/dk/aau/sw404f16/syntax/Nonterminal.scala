package dk.aau.sw404f16.syntax
import dk.aau.sw404f16.VariableList
import dk.aau.sw404f16.util._
import dk.aau.sw404f16.util.Extensions._

/**
  * Created by coffee on 4/5/16.
  */
// Program
case class Program(moduleName: ModuleName, constructors: List[TopLevelCons]) extends ASTNode
case class ModuleName(identifiers: List[Identifier]) extends ASTNode

// Top-Level Constructors
case class ModuleImport(module: ModuleName) extends TopLevelCons
case class ActorDefinition(primaryType: TypeDefinition, inheritedType: Option[List[TypeDefinition]],
                           body: ActorBodyBlock) extends ActorVariant // actor variant is a top-level constructor

case class ReceiverDefinition(primaryType: TypeDefinition, inheritedType: Option[List[TypeDefinition]],
                              body: ActorBodyBlock) extends ActorVariant

// Type Definition
case class TypeDefinition(identifier: Identifier, optionalType: Option[List[TypeParameter]]) extends ASTNode
case class TypeParameter(typeDef: Either[TypeDefinition, Identifier]) extends ASTNode

// Actors and Messages
case class ActorBodyBlock(msgs: List[MessageDefinition]) extends ASTNode
case class MessageDefinition(typeDef: TypeDefinition, pattern: PatternDefinition, block: Block) extends ASTNode
case class PatternDefinition(pattern: Either[Literal, TypedValue]) extends ASTNode {
  override def toElixir: String = pattern match {
    case Left(lit)  => lit.toElixir
    case Right(tId) => tId.toElixir
  }
}
case class TypedValue(typeDef: TypeDefinition, id: Identifier) extends ASTNode {
  override def toElixir: String = id.toElixir
  def toTuple: (String, String) = (typeDef.toElixir, id.toElixir)
}

// Data Structure
case class DataStructureDefinition(typeDef: TypeDefinition, optionalInheritedTypes: Option[List[TypeDefinition]],
                                   dataBlock: DataBodyBlock) extends TopLevelCons {
  override def toElixir: String = ???
}
case class DataBodyBlock(optionalFields: Option[FieldDefinitions])
case class FieldDefinitions(patterns: List[TypedValue]) extends ASTNode {
  override def toElixir: String = patterns.map(x => s"${x.toElixir}: nil").mkString(", ")
}

case class Block(data: List[Statement]) extends Expression {
  override def toElixir: String = data.map(_.toElixir).mkString("\n")
}

// Expressions and statements
case class Statement(stmt: Either3[Expression, ValueDefinition, BinaryOperation]) extends ASTNode {
  override def toElixir: String = stmt match {
    case Top(expr)      => expr.toElixir
    case Middle(valDef) => valDef.toElixir
    case Bottom(binOp)  => binOp.toElixir
  }
}

case class ListLiteral(expressions: List[Expression]) extends Literal {
  override def toElixir: String = "[" + expressions.map(_.toElixir).mkString(", ") + "]"
}

// values and functions
case class ValueDefinition(valueIdentifier: Either[Identifier, TypedValue], expression: Expression) extends ASTNode {
  override def toElixir: String = {
    val expr = expression.toElixir
    val id = valueIdentifier match {
      case Left(id) => id.toElixir
      case Right(tId) => tId.toElixir
    }

//    if (VariableList contains id)
//      s"^$id = $expr"
//    else {
      VariableList addValue id // add value to symbol table
      s"$id = $expr"
//    }
  }
}
case class FunctionDefinition(optionalId: Option[Identifier], arguments: List[TypedValue], block: Block) extends ASTNode {
  override def toElixir: String = {
    val eArgs = arguments.mkElixir
    val args = "(" + eArgs.mkString(", ") + ")"

    VariableList.addScope() // enter new scope before adding args
    eArgs.foreach(VariableList.addValue) // add all the args to the symbol table

    optionalId match {
      case Some(id) => s"def $id$args do ${block.toElixir} end"
      case None     => s"fn $args -> ${block.toElixir} end"
    }
  }
}
case class FunctionCall(identifier: Identifier, arguments: List[Expression]) extends Expression
case class MethodCall(obj: Identifier, function: FunctionCall) extends Expression
case class FieldCall(obj: Identifier, field: Identifier) extends Expression

case class BinaryOperation(lhs: Expression, operator: Operator, rhs: Expression) extends Expression {
  override def toElixir: String = {
    val (left, op, right) = (lhs.toElixir, operator.toElixir, rhs.toElixir) // 1337 h4xx0rz
    s"($left) $op ($right)" // TODO: may have to be re-written later
  }
}

// tell and ask
case class TellStatement(targets: List[Expression], messages: List[Expression]) extends ASTNode {
  override def toElixir: String = {
    val msgs = for {
      target <- targets
      message <- messages
    } yield s"send ${target.toElixir}, ${message.toElixir}"

    msgs mkString "\n"
  }
}
case class AskStatement(targets: List[Expression], messages: List[Expression]) extends Expression {
  override def toElixir: String = ???
}

// if-statement
case class IfExpression(statements: List[IfStatement]) extends Expression {
  override def toElixir: String = {
    val start = "cond do"
    val middle = statements.mkElixirString("\n")
    s"$start \n$middle \nend\n"
  }
}
case class IfStatement(boolean: Statement, body: Expression) extends ASTNode {
  override def toElixir: String = boolean.toElixir + " -> " + body.toElixir
}

// match-statement
case class MatchExpression(expression: Expression, statements: List[MatchStatement]) extends Expression {
  override def toElixir: String = {
    val start = "case " + expression.toElixir + " do"
    val middle = statements mkElixirString "\n"
    s"$start \n$middle \nend"
  }
}
case class MatchStatement(patternDefinition: PatternDefinition, body: Expression) extends ASTNode {
  override def toElixir: String = patternDefinition.toElixir + " -> " + body.toElixir
}

// for-comprehension. TODO: figure out how to represent this in elixir
case class ForComprehension(forBlock: List[ForStatement], doOrYield: Either[Do.type, Yield.type], block: Block) extends Expression {
  override def toElixir: String = {
    val keyword = "for "
    val iterations = forBlock.mkElixirString(",\n")
    val end = s", do: ${block.toElixir}"
    keyword + iterations + end // TODO: find out if ", do: block.toElixir" is the best thing to do.
  }
}
case class ForStatement(identifier: Identifier, expression: Expression) extends ASTNode {
  override def toElixir: String = s"${identifier.toElixir} <- ${expression.toElixir}"
}

case class AtomConstruct(atom: Atom, optionalArgs: Option[List[Expression]]) extends Literal {
  override def toElixir: String = optionalArgs match {
    // if the atom doesn't have arguments, return the atom itself
    case None => atom.toElixir

    // if the atom DOES have arguments, return a tuple of the form {:atom, arg1, arg2, etc}
    case Some(args) =>
      val arguments = args.mkElixirString
      s"{${atom.toElixir}, $arguments}"
  }
}