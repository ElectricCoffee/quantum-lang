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
  def program = "module" ~> moduleName ~ ";" ~ topLevelCons ^^ {
    case m ~ ";" ~ constructors => ???
  }

  def moduleName = Regexp.idTok ~ rep("." ~ Regexp.idTok) ^^ {
    case id ~ list => list.foldLeft(id) {
      case (x, "." ~ y) => ???
    }
  }

  def topLevelCons = rep(moduleImport | actorDef | dataStructDef) ^^ {_ => ???}

  def moduleImport = "import" ~> moduleName <~ ";" ^^ {
    case mName => ???
  }

  def actorDef = ("actor" | "receiver") ~ typeDef ~ opt("<-" ~> typeDefs) ~ actorBodyBlock ^^ {
    case "actor" ~ typeDef ~ optionalTypes ~ body => optionalTypes match {
      case Some(typeDefs) => ???
      case None => ???
    }
    case "receiver" ~ typeDef ~ optionalTypes ~ body => optionalTypes match {
      case Some(typeDefs) => ???
      case None => ???
    }
  }
  def typeDefs = typeDef ~ rep("," ~> typeDef) ^^ {
    case typeDef ~ typeList => ???
  }

  def typeDef = Regexp.idTok ~ opt("of" ~> typeParams) ^^ {
    case id ~ optional => optional match {
      case Some(typeParams) => ???
      case None => ???
    }
  }

  def typeParams = typeParam ~ rep("," ~> typeParam) ^^ {
    case typeParameter ~ typeParameters => ???
  }

  def typeParam  = "(" ~> typeDef <~ ")" | Regexp.idTok ^^ {
    case typeDefinition => ???
    case identifier => ???
  }

  def actorBodyBlock = "{" ~> rep(messageDef) <~ "}" ^^ {
    case messageDefinitions => messageDefinitions.map {
      case messageDefinition => ???
    }
  }

  def messageDef = "define" ~> typeDef ~ patternDef ~ "=" ~ block ^^ {
    case typeDefinition ~ pattern ~ "=" ~ codeBlock => ???
  }

  def patternDef = lit | "(" ~> patternVal <~ ")" ^^ {
    case literal => ???
    case pattern => ???
  }

  def patternVal = typeDef ~ Regexp.idTok ^^ {
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

  def stmt = (expr | Regexp.idTok) <~ ";" ^^ {
    case expression => ???
    case identifier => ???
  }

  def valDef = "val" ~> (Regexp.idTok | patternVal) ~ "=" ~ expr ^^ {
    case identifier ~ "=" ~ expression => ???
    case pattern ~ "=" ~ expression => ???
  }

  def funDef = "func" ~> opt(Regexp.idTok) ~ "=" ~ block ^^ {
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
  def forStmt  = Regexp.idTok ~ "in" ~ expr {_ => ???}

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