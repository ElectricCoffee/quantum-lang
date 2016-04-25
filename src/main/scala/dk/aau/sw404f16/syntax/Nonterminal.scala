package dk.aau.sw404f16.syntax
import dk.aau.sw404f16.util._

/**
  * Created by coffee on 4/5/16.
  */
// Program
case class Program(moduleName: ModuleName, constructors: List[TopLevelCons]) extends ASTNode
case class ModuleName(identifiers: List[Identifier]) extends ASTNode

// Top-Level Constructors
abstract class TopLevelCons extends ASTNode
case class ModuleImport(module: ModuleName) extends TopLevelCons

abstract class ActorVariant extends TopLevelCons
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
case class PatternDefinition(pattern: Either[Literal, TypedValue]) extends ASTNode
case class TypedValue(typeDef: TypeDefinition, id: Identifier) extends ASTNode

// Data Structure
case class DataStructureDefinition(typeDef: TypeDefinition, dataBlock: DataBodyBlock,
                                   optionalInheritedTypes: Option[List[TypeDefinition]]) extends TopLevelCons
case class DataBodyBlock(optionalFields: Option[FieldDefinitions])
case class FieldDefinitions(patterns: List[TypedValue]) extends ASTNode

case class Block(data: List[Statement]) extends Expression

// Expressions and statements
abstract class Expression extends ASTNode
case class Statement(stmt: Either3[Expression, ValueDefinition, BinaryOperation]) extends ASTNode

trait Literal extends Expression
case class ListLiteral(expressions: List[Expression]) extends Literal

// values and functions
case class ValueDefinition(valueIdentifier: Either[Identifier, TypedValue], expression: Expression) extends ASTNode
case class FunctionDefinition(optionalId: Option[Identifier], arguments: List[TypedValue],
                              block: Block) extends ASTNode
case class FunctionCall(identifier: Identifier, arguments: List[Expression]) extends Expression
case class MethodCall(obj: Identifier, function: FunctionCall) extends Expression
case class FieldCall(obj: Identifier, field: Identifier) extends Expression

case class BinaryOperation(lhs: Expression, operator: Operator, rhs: Expression) extends Expression

// tell and ask
case class TellStatement(targets: List[Expression], messages: List[Expression]) extends ASTNode
case class AskStatement(targets: List[Expression], messages: List[Expression]) extends Expression

// if-statement
case class IfExpression(statements: List[IfStatement]) extends Expression
case class IfStatement(boolean: Statement, body: Expression) // doesn't extend Expression, it can't stand alone

// match-statement
case class MatchExpression(expression: Expression, statements: List[MatchStatement]) extends Expression
case class MatchStatement(patternDefinition: PatternDefinition, body: Expression)

// for-comprehension
case class ForComprehension(forBlock: List[ForStatement],
                            doOrYield: Either[Do.type, Yield.type],
                            block: Block) extends Expression
case class ForStatement(identifier: Identifier, expression: Expression)

case class AtomConstruct(atom: Atom, optionalArgs: Option[List[Expression]]) extends Literal