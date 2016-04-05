package dk.aau.sw404f16.syntax.parser
import dk.aau.sw404f16.syntax.lexer.Regexp
import dk.aau.sw404f16.syntax._

import scala.util.parsing.combinator._

/**
  * Created by coffee on 3/29/16.
  * ~  means "followed by"
  * ~> means "ignore what's on the left"
  * <~ means "ignore what's on the right"
  * ^^ means "if the pattern holds, do the following"
  * ??? "means not yet implemented"
  */
object Parser extends RegexParsers {
  def program: Parser[Program] = "module" ~> moduleName ~ ";" ~ topLevelCons ^^ {
    case m ~ ";" ~ constructors => Program(m, constructors)
  }
  def identifier: Parser[Identifier] = Regexp.idTok ^^ Identifier

  def moduleName: Parser[ModuleName] = identifier ~ rep("." ~> identifier) ^^ {
    case id ~ list => ModuleName(id :: list)
  }

  def topLevelCons: Parser[List[TopLevelCons]] = rep(moduleImport | actorDef | dataStructDef) ^^ {
    _.map {
      case mImport => ???
      case aDef => ???
      case dsDef => ???
    }
  }

  def moduleImport: Parser[ModuleImport] = "import" ~> moduleName <~ ";" ^^ ModuleImport

  def actorDef: Parser[ActorVariant] =
    ("actor" | "receiver") ~ typeDef ~ opt("<-" ~> typeDefs) ~ actorBodyBlock ^^ {
      case "actor" ~ typeDef ~ optionalTypes ~ body => ActorDefinition(typeDef, optionalTypes, body)
      case "receiver" ~ typeDef ~ optionalTypes ~ body => ReceiverDefinition(typeDef, optionalTypes, body)
    }

  def typeDefs: Parser[TypeDefinitions] = typeDef ~ rep("," ~> typeDef) ^^ {
    case typeDef ~ typeList => TypeDefinitions(typeDef :: typeList)
  }

  def typeDef: Parser[TypeDefinition] = identifier ~ opt("of" ~> typeParams) ^^ {
    case id ~ optional => TypeDefinition(id, optional)
  }

  def typeParams = typeParam ~ rep("," ~> typeParam) ^^ {
    case head ~ tail => TypeParameters(head :: tail)
  }

  def typeParam: Parser[TypeParameter] = "(" ~> typeDef <~ ")" ^^ {
    td => TypeParameter(Left(td))
  } | identifier ^^ { // changing the formatting might break the code, so please don't touch
    id => TypeParameter(Right(id))
  }

  def actorBodyBlock: Parser[List[MessageDefinition]] = "{" ~> rep(messageDef) <~ "}" ^^ {
    case messageDefinitions => messageDefinitions
  }

  def messageDef: Parser[MessageDefinition] = "define" ~> typeDef ~ patternDef ~ "=" ~ block ^^ {
    case typeDefinition ~ pattern ~ "=" ~ codeBlock =>
      MessageDefinition(typeDefinition, pattern, codeBlock)
  }

  def patternDef: Parser[PatternDefinition] = lit ^^ {
    lit => ???
  } | "(" ~> patternVal <~ ")" ^^ {
    patt => ???
  }

  def patternVal = typeDef ~ identifier ^^ {
    case typeDefinition ~ identifier => ???
  }

  def dataStructDef = "data" ~> typeDef ~ dataBodyBlock ~ opt("<-" ~> typeDefs) ^^ {
    case typeDefinition ~ dataBlock ~ optionalTypeDefs => optionalTypeDefs match {
      case Some(typeDefs) => ???
      case None => ???
    }
  }

  def dataBodyBlock = opt("{" ~> fieldDefs <~ "}") ^^ {
    case Some(fieldDefs) => ???
    case None => ???
  }

  def fieldDefs = rep(patternVal <~ ";") ^^ {
    _.map { // _.map is shorthand for "case x => x.map"
      case pattern => ???
    }
  }

  def block = "{" ~> stmts <~ "}" | stmt ^^ {
    case statements => ???
    case statement => ???
  }

  def stmts = rep1(stmt) ^^ {
    _.map {
      case statement => ???
    }
  }

  def stmt = (expr | identifier) <~ ";" ^^ {
    case expression => ???
    case identifier => ???
  }

  def valDef = "val" ~> (identifier | patternVal) ~ "=" ~ expr ^^ {
    case identifier ~ "=" ~ expression => ???
    case pattern ~ "=" ~ expression => ???
  }

  def funDef = "func" ~> opt(identifier) ~ "=" ~ block ^^ {
    case optionalId ~ "=" ~ codeBlock => optionalId match {
      case Some(identifier) => ???
      case None => ???
    }
  }

  def expr   = /*boolExpr | numExpr |*/ ifExpr | forCompr | matchExpr | lit ^^ {_ => ???}

  def decLit = Regexp.numLitTok ~ "." ~ Regexp.numLitTok ^^ {
    case decimal ~ "." ~ mantissa => NumberLiteral(s"$decimal.$mantissa")
  }

  def tellStmt = "tell" ~> neArgs ~ "about" ~ neArgs <~ ";" ^^ {
    case target ~ "about" ~ message => ???
  }

  def askStmt  = "ask" ~> neArgs ~ "about" ~ neArgs <~ ";" ^^ {
    case target ~ "about" ~ message => ???
  }

  def neArgs = expr ~ rep("," ~> expr) ^^ {
    case expression ~ exprList => ???
  }

  def ifExpr = "if" ~ ifBlock ^^ {_ => ???}
  def ifBlock = "{" ~ ifStmts ~ "}" | ifStmt {_ => ???}
  def ifStmts = rep1(ifStmt) {_ => ???}
  def ifStmt = boolExpr ~ "then" ~ expr <~ ";" {_ => ???}

  def matchExpr = "match" ~ "(" ~> expr ~ ")" ~ matchBlock ^^ {_ => ???}
  def matchBlock = "{" ~> matchStmts <~ "}" | matchStmt {_ => ???}
  def matchStmts = rep1(matchStmt) {_ => ???}
  def matchStmt  = patternval ~ "then" ~ expr <~ ";" {_ => ???}

  def forCompr = "for" ~> forBlock ~ expr ^^ {_ => ???}
  def forBlock = "{" ~>  forStmts <~ "}" | forStmt {_ => ???}
  def forStmts = rep1(forStmt) {_ => ???}
  def forStmt  = identifier ~ "in" ~ expr {_ => ???}

  def list = "[" ~> args <~ "]" ^^ {_ => ???}
  def neArgs = expr ~ rep("," ~> expr) ^^ {
    case expression ~ exprList => ???
  }
  def args = opt(neArgs) ^^ {
    case Some(args) => ???
    case None => Nil // Nil = empty list, not null
  }
  def lit = Regexp.stringLitTok | Regexp.numLitTok | decLit |
    Regexp.atomTok ~ opt("(" ~> args <~ ")") | list ^^ {
    case stringLiteral => ???
    case integerLiteral => ???
    case decimalLiteral => ???
    case atomLiteral ~ optionalArguments => ???
    case listLiteral => ???
  }
}