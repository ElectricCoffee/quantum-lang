package dk.aau.sw404f16.syntax

/**
  * Created by coffee on 4/5/16.
  */
trait Nonterminal extends ASTNode
abstract class TopLevelCons extends Nonterminal
abstract class ActorVariant extends TopLevelCons
abstract class Literal extends Nonterminal
abstract class Expression extends Nonterminal

case class Program(moduleName: ModuleName, constructors: List[TopLevelCons]) extends Nonterminal
case class ModuleName(identifiers: List[Identifier]) extends Nonterminal

case class ModuleImport(module: ModuleName) extends Nonterminal

case class ActorDefinition(primaryType: TypeDefinition,
                           inheritedType: Option[TypeDefinitions],
                           body: ActorBodyBlock) extends ActorVariant

case class ReceiverDefinition(primaryType: TypeDefinition,
                              inheritedType: Option[TypeDefinitions],
                              body: ActorBodyBlock) extends ActorVariant

case class TypeDefinitions(definitions: List[TypeDefinition]) extends Nonterminal
case class TypeDefinition(identifier: Identifier, optionalType: Option[TypeParameters]) extends Nonterminal
case class TypeParameters(params: List[TypeParameter]) extends Nonterminal
case class TypeParameter(typeDef: Either[TypeDefinition, Identifier]) extends Nonterminal

case class ActorBodyBlock(msgs: List[MessageDefinition]) extends Nonterminal
case class MessageDefinition(typeDef: TypeDefinition, pattern: PatternDefinition, block: Block) extends Nonterminal
case class PatternDefinition(pattern: Either[Literal, PatternValue]) extends Nonterminal
case class PatternValue(typeDef: TypeDefinition, id: Identifier) extends Nonterminal
case class DataStructureDefinition(typeDef: TypeDefinition,
                                   dataBlock: DataBodyBlock,
                                   optionalInheritedTypes: Option[TypeDefinitions]) extends Nonterminal
case class DataBodyBlock(optionalFields: Option[FieldDefinitions]) extends Nonterminal
case class FieldDefinitions(patterns: List[PatternValue]) extends Nonterminal
case class Block(data: Either[Statements, Statement]) extends Nonterminal

case class Statements(stmts: List[Statement]) extends Nonterminal
case class Statement(stmt: Either[Expression, Identifier]) extends Nonterminal

case class ValueDefinition(valueIdentifier: Either[Identifier, PatternValue], expression: Expression) extends Nonterminal
case class FunctionDefinition(optionalId: Option[Identifier], block: Block)