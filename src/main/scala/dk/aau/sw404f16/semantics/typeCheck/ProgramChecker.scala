package dk.aau.sw404f16.semantics.typeCheck

import dk.aau.sw404f16.syntax._
import dk.aau.sw404f16.util.{Bottom, Middle, Top}

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

  def checkActorBody(body: ActorBodyBlock) = body.msgs.map {
    case MessageDefinition(typeDef, pattern, block) => ???
  }

  def checkBlock(block: Block) = block.data.map(checkStatement)

  def checkStatement(statement: Statement) = statement.stmt match {
    case Top(expr)      => ???
    case Middle(valDef) => ???
    case Bottom(binOp)  => ???
  }

  def checkValue(valDef: ValueDefinition) = {
    val ValueDefinition(value, expr) = valDef // easy way to extract data
    value match {
      case Left(Identifier(id)) => ???
      case Right(TypedValue(typeDef, id)) => ???
    }
  }
}
