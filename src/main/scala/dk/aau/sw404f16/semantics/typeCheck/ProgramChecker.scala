package dk.aau.sw404f16.semantics.typeCheck

import dk.aau.sw404f16.semantics.TypeInfo
import dk.aau.sw404f16.semantics.exceptions.TypeMismatchException
import dk.aau.sw404f16.syntax._
import dk.aau.sw404f16.util.Convenience.lineRef

// shortening some names
import dk.aau.sw404f16.syntax.{
  TypeDefinition    => TypeDef,
  ActorBodyBlock    => ActorBody,
  DataBodyBlock     => DataBody,
  MessageDefinition => MsgDef,
  ValueDefinition   => ValDef
}

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

  def checkActorDef(primTyp: TypeDef, inhTyp: Option[List[TypeDef]], body: ActorBody): TypeInfo = {
    evalActorBodyTypes(body) // TODO: store and define all the message types in the symbol table
    val primary = primTyp.toTypeInfo
    inhTyp match {
      case Some(bases) => primary makeSubTypeOf bases.map(_.toTypeInfo)
      case None        => primary
    }
    // TODO: store this information in the symbol table
  }

  def evalActorBodyTypes(body: ActorBody): Unit = {
    val errors = body.msgs.map {
      case MessageDefinition(typeDef, pattern, block) =>
        val returnType = typeDef.toTypeInfo
        val blockType = ExpressionChecker check block
        if (returnType <!=> blockType && returnType <!^=> blockType)
          s"Type Mismatch: the body of $pattern is not of type $returnType"
        else ""
    }.filter(_ != "")

    if (errors.nonEmpty)
      throw TypeMismatchException(errors mkString ", ")

    // TODO: if all succeeds, store the actor definition in the symbol table
  }

  def checkStructDef(primTyp: TypeDef, inhTyp: Option[List[TypeDef]], body: DataBody): TypeInfo = {
    evalStructBody(body) // TODO: store and define all the message types in the symbol table
    val primary = primTyp.toTypeInfo
    inhTyp match {
      case Some(bases) => primary makeSubTypeOf bases.map(_.toTypeInfo)
      case None        => primary
    }
    // TODO: store this information in the symbol table
  }

  def evalStructBody(body: DataBody): Unit = body.optionalFields match {
    case Some(FieldDefinitions(patterns)) => patterns.map {
      case TypedValue(typeDef, id) => ???
    }
    case None => // do nothing
  }

  def checkMsgDef(msgDef: MsgDef) = msgDef match {
    case MessageDefinition(typeDef, pattern, block) =>
      // TODO: add the type-definition to the symbol table, and link its block
      val returnType = typeDef.nodeType
      val blockType  = ExpressionChecker.check(block)
      if(returnType <!=> blockType || returnType <!?=> blockType)
        throw TypeMismatchException(s"Type of message ${lineRef(typeDef)} does not match the type of its associated block")
  }

  def checkValue(valDef: ValDef) = {
    val ValueDefinition(value, expr) = valDef // easy way to extract data
    value match {
      case Left(Identifier(id)) => ???
      case Right(TypedValue(typeDef, id)) => ???
    }
  }
}
