package dk.aau.sw404f16.syntax.parser
import dk.aau.sw404f16.syntax.Identifier
import dk.aau.sw404f16.syntax.lexer.Regexp

import scala.util.parsing.combinator._

/**
  * Created by coffee on 3/29/16.
  */
object Parser extends RegexParsers {
  def program = "module" ~ moduleName ~ ";" ~ topLevelCons ^^ {_ => ???}
  def moduleName = Regexp.idTok ~ rep("." ~ Regexp.idTok) ^^ {_ => ???}
  def topLevelCons = rep(moduleImport | actorDef | dataStructDef) ^^ {_ => ???}
  def moduleImport = "import" ~ moduleName ~ ";" ^^ {_ => ???}
  def actorDef = ("actor" | "receiver") ~ typeDef ~ opt("<-" ~ typeDefs) ~ actorBodyBlock ^^ {_ => ???}
  def typeDefs = typeDef ~ rep("," ~ typeDef) ^^ {_ => ???}
  def typeDef = Regexp.idTok ~ "of" ~ typeParams | Regexp.idTok ^^ { _ => ???}
  def typeParams = typeParam ~ rep("," ~ typeParam) ^^ {_ => ???}
  def typeParam  = "(" ~ typeDef ~ ")" | Regexp.idTok ^^ {_ => ???}
  def actorBodyBlock = "{" ~ actorBodyDef ~ "}" ^^ {_ => ???}
  def actorBodyDef = rep(messageDef) ^^ {_ => ???}
  def messageDef = "define" ~ typeDef ~ patternDef ~ "=" ~ block ^^ {_ => ???}
  def patternDef = lit | "(" ~> patternVal <~ ")" ^^ {_ => ???}
  def patternVal = typeDef ~ Regexp.idTok ^^ {_ => ???}
  def dataStructDef = "data" ~ typeDef ~ dataBodyBlock ~ opt("<-" ~ typeDefs) ^^ {_ => ???}
  def dataBodyBlock = opt("{" ~ fieldDefs ~ "}") ^^ {_ => ???}
  def fieldDefs = rep(patternVal ~ ";") ^^ {_ => ???}
  def block = "{" ~ stmts ~ "}" | stmt ^^ {_ => ???}
  def stmts = rep1(stmt) ^^ {_ => ???}
  def stmt = expr ~ ";" | Regexp.idTok ^^ {_ => ???}
  def valDef = "val" ~ (Regexp.idTok | patternVal) ~ "=" ~ expr ^^ {_ => ???}
  def funDef = "func" ~ opt(Regexp.idTok) ~ "=" ~ block ^^ {_ => ???}
  def expr   = /*boolExpr | numExpr |*/ ifExpr | forCompr | matchExpr | lit ^^ {_ => ???}
  def decLit = Regexp.numLitTok ~ "." ~ Regexp.numLitTok ^^ {_ => ???}
  def tellStmt = "tell" ~ neArgs ~ "about" ~ neArgs ~ ";" ^^ {_ => ???}
  def askStmt  = "ask"  ~ neArgs ~ "about" ~ neArgs ~ ";" ^^ {_ => ???}
  def neArgs = expr ~ rep("," ~ expr) ^^ {_ => ???}

  def ifExpr = "if" ~ ifBlock ^^ {_ => ???}
  def ifBlock = "{" ~ ifStmts ~ "}" | ifStmt
  def ifStmts = rep1(ifStmt)
  def ifStmt = boolExpr ~ "then" ~ expr ~ ";"

  def matchExpr = "match" ~ "(" ~ expr ~ ")" ~ matchBlock ^^ {_ => ???}
  def matchBlock = "{" ~ matchStmts ~ "}" | matchStmt
  def matchStmts = rep1(matchStmt)
  def matchStmt  = patternval ~ "then" ~ expr ~ ";"

  def forCompr = "for" ~ forBlock ~ expr ^^ {_ => ???}
  def forBlock = "{" ~  forStmts ~ "}" | forStmt
  def forStmts = rep1(forStmt)
  def forStmt  = Regexp.idTok ~ "in" ~ expr

  def list = "[" ~ args ~ "]" ^^ {_ => ???}
  def neArgs = expr ~ rep("," ~ expr) ^^ {_ => ???}
  def args = opt(neArgs) ^^ {_ => ???}
  def lit = Regexp.stringLitTok | Regexp.numLitTok | decLit |
    Regexp.atomTok ~ opt("(" ~ args ~ ")") | list ^^ {_ => ???}

}