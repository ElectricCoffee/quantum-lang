package dk.aau.sw404f16.syntax

/**
  * Created by coffee on 4/5/16.
  */
trait Nonterminal extends ASTNode
abstract class TopLevelCons extends Nonterminal
abstract class ActorVariant extends TopLevelCons

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