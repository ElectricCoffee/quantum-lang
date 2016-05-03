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
    case ReceiverDefinition(primary, inherited, body)      =>  checkActorDef(primary, inherited, body)
    case ActorDefinition(primary, inherited, body)         =>  checkActorDef(primary, inherited, body)
    case DataStructureDefinition(primary, inherited, data) => checkStructDef(primary, inherited, data)
  }

  def checkActorDef(primTyp: TypeDefinition, inhTyp: Option[List[TypeDefinition]], body: ActorBodyBlock): TypeInfo = {
    evalActorBody(body) // TODO: store and define all the message types in the symbol table
    val primary = primTyp.toTypeInfo
    inhTyp match {
      case Some(bases) => primary makeSubTypeOf bases.map(_.toTypeInfo)
      case None        => primary
    }
    // TODO: store this information in the symbol table
  }

  def evalActorBody(body: ActorBodyBlock): Unit = body.msgs.map {
    case MessageDefinition(typeDef, pattern, block) => ???
  }

  def checkStructDef(primTyp: TypeDefinition, inhTyp: Option[List[TypeDefinition]], body: DataBodyBlock): TypeInfo = {
    evalStructBody(body) // TODO: store and define all the message types in the symbol table
    val primary = primTyp.toTypeInfo
    inhTyp match {
      case Some(bases) => primary makeSubTypeOf bases.map(_.toTypeInfo)
      case None        => primary
    }
    // TODO: store this information in the symbol table
  }

  def evalStructBody(body: DataBodyBlock): Unit = body.optionalFields match {
    case Some(FieldDefinitions(patterns)) => patterns.map {
      case TypedValue(typeDef, id) => ???
    }
    case None => // do nothing
  }

  def checkMsgDef(msgDef: MessageDefinition) = msgDef match {
    case MessageDefinition(typeDef, pattern, block) =>
      // TODO: add the type-definition to the symbol table, and link its block
      val returnType = typeDef.nodeType
      val blockType  = ExpressionChecker.check(block)
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
