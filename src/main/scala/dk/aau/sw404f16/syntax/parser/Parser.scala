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
  * ??? means "not yet implemented"
  * rep() means "repeat 0 or more times", returns a List[T]
  * rep1() means "repeat 1 or mote times" returns a List[T]
  * opt() means "optional" returns an Option[T]
  */
object Parser extends RegexParsers {
  def program: Parser[Program] = "module" ~> moduleName ~ ";" ~ topLevelCons ^^ {
    case m ~ ";" ~ constructors => Program(m, constructors)
  }

  def identifier: Parser[Identifier] = Regexp.idTok ^^ Identifier
  def stringLiteral: Parser[StringLiteral] = Regexp.stringLitTok ^^ StringLiteral
  def numberLiteral: Parser[NumberLiteral] = Regexp.numLitTok ^^ NumberLiteral
  def assignment: Parser[Assignment.type]  = Regexp.assignTok ^^^ Assignment
  def operator:   Parser[Operator]         = Regexp.operatorTok ^^ Operator

  // TODO: Find better solution
  def atom: Parser[Either[Atom, (Atom, Args)]] = Regexp.atomTok ~ opt("(" ~> arguments <~ ")") ^^ {
    case atomToken ~ args => args match {
      case Some(arguments) => Right((Atom(atomToken), args))
      case None => Left(Atom(atomToken))
    }
  }

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

  def typeParam: Parser[TypeParameter] =
    "(" ~> typeDef <~ ")" ^^ {
      td => TypeParameter(Left(td))
    } | identifier ^^ { // changing the formatting might break the code, so please don't touch
      id => TypeParameter(Right(id))
    }

  def actorBodyBlock: Parser[ActorBodyBlock] = "{" ~> rep(messageDef) <~ "}" ^^ {
    case messageDefinitions => ActorBodyBlock(messageDefinitions)
  }

  def messageDef: Parser[MessageDefinition] = "define" ~> typeDef ~ patternDef ~ "=" ~ block ^^ {
    case typeDefinition ~ pattern ~ "=" ~ codeBlock =>
      MessageDefinition(typeDefinition, pattern, codeBlock)
  }

  def patternDef: Parser[PatternDefinition] =
    lit ^^ {
      lit => ???
    } | "(" ~> patternVal <~ ")" ^^ {
      patt => ???
    }

  def patternVal: Parser[PatternValue] = typeDef ~ identifier ^^ {
    case typeDefinition ~ identifier => PatternValue(typeDefinition, identifier)
  }

  def dataStructDef: Parser[DataStructureDefinition] =
    "data" ~> typeDef ~ dataBodyBlock ~ opt("<-" ~> typeDefs) ^^ {
      case typeDefinition ~ dataBlock ~ optionalTypeDefs =>
        DataStructureDefinition(typeDefinition, dataBlock, optionalTypeDefs)
    }

  def dataBodyBlock: Parser[DataBodyBlock] = opt("{" ~> fieldDefs <~ "}") ^^ DataBodyBlock

  def fieldDefs: Parser[FieldDefinitions] = rep(patternVal <~ ";") ^^ FieldDefinitions

  def block: Parser[Block] = "{" ~> stmts <~ "}" ^^ {
    ss => Block(Left(ss))
  } | stmt ^^ {
    s  => Block(Right(s))
  }

  def stmts: Parser[Statements] = rep1(stmt) ^^ Statements

  def stmt: Parser[Statement] = (expr | identifier) <~ ";" ^^ {
    case expression: Expression => Statement(Left(expression))
    case identifier: Identifier => Statement(Right(identifier))
  }

  def valDef: Parser[ValueDefinition] = "val" ~> (identifier | patternVal) ~ "=" ~ expr ^^ {
    case (identifier: Identifier) ~ "=" ~ expression =>
      ValueDefinition(Left(identifier), expression)
    case (pattern: PatternValue) ~ "=" ~ expression =>
      ValueDefinition(Right(pattern), expression)
  }

  def funDef: Parser[FunctionDefinition] = "func" ~> opt(identifier) ~ "=" ~ block ^^ {
    case optionalId ~ "=" ~ codeBlock => FunctionDefinition(optionalId, codeBlock)
  }

  // TODO: add binary operation expression
  def binaryOperation = expr ~ operator ~ expr ^^ {_ => ???}

  def expr: Parser[Expression]  = binaryOperation | ifExpr | forCompr | matchExpr | lit ^^ {
    case binaryExpression => ???
    case ifExpression => ???
    case forComprehension => ???
    case matchExpression => ???
    case literal => ???
  }

  def tellStmt = "tell" ~> neArgs ~ "about" ~ neArgs <~ ";" ^^ {
    case target ~ "about" ~ message => ???
  }

  def askStmt  = "ask" ~> neArgs ~ "about" ~ neArgs <~ ";" ^^ {
    case target ~ "about" ~ message => ???
  }

  // must have at least one argument
  def nonemptyArguments: Parser[List[Expression]] = expr ~ rep("," ~> expr) ^^ {
    case expression ~ exprList => expression :: exprList
  }

  // can be empty
  def arguments: Parser[List[Expression]] = opt(neArgs) ^^ {
    case Some(args) => args
    case None       => Nil // Nil = empty list, not null
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

  def list = "[" ~> arguments <~ "]" ^^ { _ => ???}
  def neArgs = expr ~ rep("," ~> expr) ^^ {
    case expression ~ exprList => ???
  }

  def lit = stringLiteral | numberLiteral |
    Regexp.atomTok ~ opt("(" ~> arguments <~ ")") | list ^^ {
    case stringLiteral => ???
    case numericLiteral => ???
    case atomLiteral ~ optionalArguments => ???
    case listLiteral => ???
  }
}