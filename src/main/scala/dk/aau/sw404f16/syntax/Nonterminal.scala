package dk.aau.sw404f16.syntax

/**
  * Created by coffee on 4/5/16.
  */
trait Nonterminal extends ASTNode

// Program
case class Program(moduleName: ModuleName, constructors: List[TopLevelCons]) extends Nonterminal
case class ModuleName(identifiers: List[Identifier]) extends Nonterminal

// Top-Level Constructors
abstract class TopLevelCons extends Nonterminal
case class ModuleImport(module: ModuleName) extends TopLevelCons

abstract class ActorVariant extends TopLevelCons
case class ActorDefinition(primaryType: TypeDefinition, inheritedType: Option[List[TypeDefinition]],
                           body: ActorBodyBlock) extends ActorVariant // actor variant is a top-level constructor

case class ReceiverDefinition(primaryType: TypeDefinition, inheritedType: Option[List[TypeDefinition]],
                              body: ActorBodyBlock) extends ActorVariant

// Type Definition
case class TypeDefinition(identifier: Identifier, optionalType: Option[List[TypeParameter]]) extends Nonterminal
case class TypeParameter(typeDef: Either[TypeDefinition, Identifier]) extends Nonterminal

// Actors and Messages
case class ActorBodyBlock(msgs: List[MessageDefinition]) extends Nonterminal
case class MessageDefinition(typeDef: TypeDefinition, pattern: PatternDefinition, block: Block) extends Nonterminal
case class PatternDefinition(pattern: Either[Literal, PatternValue]) extends Nonterminal
case class PatternValue(typeDef: TypeDefinition, id: Identifier) extends Nonterminal

// Data Structure
case class DataStructureDefinition(typeDef: TypeDefinition, dataBlock: DataBodyBlock,
                                   optionalInheritedTypes: Option[List[TypeDefinition]]) extends TopLevelCons
case class DataBodyBlock(optionalFields: Option[FieldDefinitions])
case class FieldDefinitions(patterns: List[PatternValue]) extends Nonterminal

case class Block(data: List[Statement]) extends Nonterminal

// Expressions and statements
abstract class Expression extends Nonterminal
case class Statement(stmt: Either[Expression, Identifier]) extends Nonterminal

trait Literal extends Expression
case class ListLiteral(expressions: List[Expression]) extends Literal

// values and functions
case class ValueDefinition(valueIdentifier: Either[Identifier, PatternValue], expression: Expression) extends Nonterminal
case class FunctionDefinition(optionalId: Option[Identifier], arguments: List[PatternValue],
                              block: Block) extends Nonterminal

case class BinaryOperation(lhs: Expression, operator: Operator, rhs: Expression) extends Expression

// tell and ask
case class TellStatement(targets: List[Expression], messages: List[Expression]) extends Nonterminal
case class AskStatement(targets: List[Expression], messages: List[Expression]) extends Expression

// if-statement
case class IfExpression(statements: List[IfStatement]) extends Expression
case class IfStatement(boolean: Expression, body: Expression) // doesn't extend anything, it can't stand alone

// match-statement
case class MatchExpression(expression: Expression, statements: List[MatchStatement]) extends Expression
case class MatchStatement(patternValue: PatternValue, body: Expression)

// for-comprehension
case class ForComprehension(forBlock: List[ForStatement], doOrYield: Either[Do.type, Yield.type],
                            block: Block) extends Expression
case class ForStatement(identifier: Identifier, expression: Expression)

case class AtomConstruct(atom: Atom, optionalArgs: Option[List[Expression]])