package dk.aau.sw404f16.semantics.typeCheck

import dk.aau.sw404f16.semantics.TypeInfo
import dk.aau.sw404f16.semantics.exceptions.TypeMismatchException
import dk.aau.sw404f16.syntax._
import dk.aau.sw404f16.util.Convenience.lineRef

/**
  * Created by coffee on 4/15/16.
  */
object ProgramChecker {
  def checkProgram(program: Program) = program.constructors.map {
    // name @ Pattern is a way to bind a name to a pattern
    case module @ ModuleImport(moduleName) => ??? // TODO: Define module in symbol table
    case ReceiverDefinition(primary, inherited, body) => checkActorDef(primary, inherited, body)
    case ActorDefinition(primary, inherited, body)    => checkActorDef(primary, inherited, body)
    case struct @ DataStructureDefinition(typeDef, data, inherited) => ???
  }

  def checkActorDef(primTyp: TypeDefinition, inhTyp: Option[List[TypeDefinition]], body: ActorBodyBlock): TypeInfo = {
    checkActorBody(body) // TODO: store and define all the message types in the symbol table
    val primary = primTyp.toTypeInfo
    inhTyp match {
      case Some(inhs) =>
        val supers = inhs.map(_.toTypeInfo)
        primary makeSubTypeOf supers
      case None       => primary
    }
    // TODO: store this information in the symbol table
  }

  def checkActorBody(body: ActorBodyBlock): Unit = ???

  def checkMsgDef(msgDef: MessageDefinition) = msgDef match {
    case MessageDefinition(typeDef, pattern, block) =>
      // add the type-definition to the symbol table, and link its block
      val returnType = typeDef.nodeType
      val blockType  = ExpressionChecker.checkExpression(block)
      if(returnType <!=> blockType || returnType <!?=> blockType)
        throw TypeMismatchException(s"Type of message ${lineRef(typeDef)} does not match the type of its associated block")
  }

  def checkValue(valDef: ValueDefinition) = {
    val ValueDefinition(value, expr) = valDef // easy way to extract data
    value match {
      case Left(Identifier(id)) => ???
      case Right(TypedValue(typeDef, id)) => ???
    }
  }
}
