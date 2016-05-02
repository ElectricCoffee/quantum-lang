package dk.aau.sw404f16.semantics.typeCheck

import dk.aau.sw404f16.semantics.exceptions.TypeMismatchException
import dk.aau.sw404f16.syntax._
import dk.aau.sw404f16.util.Convenience.lineRef

/**
  * Created by coffee on 4/15/16.
  */
object ProgramChecker {
  def checkProgram(program: Program) = program.constructors.map {
    // name @ Pattern is a way to bind a name to a pattern
    case module @ ModuleImport(moduleName) => ???
    case receiver @ ReceiverDefinition(primary, inherited, body) => ???
    case actor @ ActorDefinition(primary, inherited, body) => ???
    case struct @ DataStructureDefinition(typeDef, data, inherited) => ???
  }

  def checkActorBody(body: ActorBodyBlock) = ???

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
