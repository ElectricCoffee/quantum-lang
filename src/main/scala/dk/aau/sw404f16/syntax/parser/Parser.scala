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
  /** any valid identifier */
  def identifier: Parser[Identifier] = Regexp.idTok ^^ Identifier
  /** a string literal with capability for escaped quotes */
  def stringLiteral: Parser[StringLiteral] = Regexp.stringLitTok ^^ StringLiteral
  /** any number; integer or float */
  def numberLiteral: Parser[NumberLiteral] = Regexp.numLitTok ^^ NumberLiteral
  /** the equals sign */
  def assignment: Parser[Assignment.type]  = Regexp.assignTok ^^^ Assignment
  /** any operator that isn't the equals sign */
  def operator:   Parser[Operator]         = Regexp.operatorTok ^^ Operator

  /** atoms are the main message-type. They can either be a stand-alone or have arguments
    * TODO: Find better representation than a tuple
    */
  def atom: Parser[Either[Atom, (Atom, Args)]] = Regexp.atomTok ~ opt("(" ~> arguments <~ ")") ^^ {
    case atomToken ~ args => args match {
      case Some(arguments) => Right((Atom(atomToken), args))
      case None => Left(Atom(atomToken))
    }
  }

  /** the outermost level of a program file
    * which is a module definition followed by some top-level constructors
    */
  def program: Parser[Program] = "module" ~> moduleName ~ ";" ~ topLevelCons ^^ {
    case m ~ ";" ~ constructors => Program(m, constructors)
  }

  /** a module name is an identifier, or multiple identifier separated by periods */
  def moduleName: Parser[ModuleName] = identifier ~ rep("." ~> identifier) ^^ {
    case id ~ list => ModuleName(id :: list)
  }

  /** top-level constructors, which include
    * module imports, actor definitions, and the definition of data structures
    * @return a parser representing a list of top-level constructors
    */
  def topLevelCons: Parser[List[TopLevelCons]] = rep(moduleImport | actorDef | dataStructDef) ^^ {
    _.map {
      case mImport => ???
      case aDef => ???
      case dsDef => ???
    }
  }

  /** module import of the form "import Example.Foo;"
    * @return a parser representation of module imports
    */
  def moduleImport: Parser[ModuleImport] = "import" ~> moduleName <~ ";" ^^ ModuleImport

  /** an actor definition comes in two flavours:
    * actor, which is an instanciable actor
    * receiver, which is a signleton actor
    * @return a parser representation of an actor-variant
    */
  def actorDef: Parser[ActorVariant] =
    ("actor" | "receiver") ~ typeDef ~ opt("<-" ~> typeDefs) ~ actorBodyBlock ^^ {
      case "actor"    ~ typeDef ~ optionalTypes ~ body => ActorDefinition(typeDef, optionalTypes, body)
      case "receiver" ~ typeDef ~ optionalTypes ~ body => ReceiverDefinition(typeDef, optionalTypes, body)
    }

  /** type-definitions are a single type-definition,
    * followed by 0 or more type-definitions seperated by commas
    * @return
    */
  def typeDefs: Parser[TypeDefinitions] = typeDef ~ rep("," ~> typeDef) ^^ {
    case typeDef ~ typeList => TypeDefinitions(typeDef :: typeList)
  }

  /** a type-definition is of one of the following forms:
    * TypeName
    * TypeName of TypeParameter
    * TypeName of (TypeName of TypeParameter)
    * @return a parser-representation of a type-definition
    */
  def typeDef: Parser[TypeDefinition] = identifier ~ opt("of" ~> typeParams) ^^ {
    case id ~ optional => TypeDefinition(id, optional)
  }

  /** type-parameters are one or more typeParam separated by commas
    * @return a parser-representation of multiple type-parameters
    */
  def typeParams = typeParam ~ rep("," ~> typeParam) ^^ {
    case head ~ tail => TypeParameters(head :: tail)
  }

  /** a single type-parameter is either
    * a type-definition in parentheses
    * or an identifier representing the type name
    * @return a parser-representation of a type-parameter
    */
  def typeParam: Parser[TypeParameter] =
    "(" ~> typeDef <~ ")" ^^ {
      td => TypeParameter(Left(td))
    } | identifier ^^ { // changing the formatting might break the code, so please don't touch
      id => TypeParameter(Right(id))
    }

  /** the body of an actor,
    * which is zero or more message definitions surrounded by braces
    * TODO: Add support for function-definitions?
    * @return a parser-representation of an actor's body-block
    */
  def actorBodyBlock: Parser[ActorBodyBlock] = "{" ~> rep(messageDef) <~ "}" ^^ {
    case messageDefinitions => ActorBodyBlock(messageDefinitions)
  }

  /** messages are defined as either
    * "define ReturnType Literal { ... }"
    * "define ReturnType (Type Variable) { ... }"
    * literals allow sending exact values to the actor,
    * while patterns allow variable data
    * @return a parser-representation of a message definition
    */
  def messageDef: Parser[MessageDefinition] = "define" ~> typeDef ~ patternDef ~ "=" ~ block ^^ {
    case typeDefinition ~ pattern ~ "=" ~ codeBlock =>
      MessageDefinition(typeDefinition, pattern, codeBlock)
  }

  /** a pattern-definition is either a
    * literal value,
    * or a type followed by an identifier in parentheses
    * see why in messageDef
    * @return parser-representation of a pattern-definition
    */
  def patternDef: Parser[PatternDefinition] =
    lit ^^ {
      literal => PatternDefinition(Left(literal))
    } | "(" ~> patternVal <~ ")" ^^ {
      pattern => PatternDefinition(Right(pattern))
    }

  /** a "pattern value" is a type followed by an identifier name.
    * it's called patternVal out of a lack for a better name
    * @return parser-representation of a pattern-value
    */
  def patternVal: Parser[PatternValue] = typeDef ~ identifier ^^ {
    case typeDefinition ~ identifier => PatternValue(typeDefinition, identifier)
  }

  /** data structures, are this language's structs/records
    * they act as the language's way to separate fields from methods
    * actors handle "method" calls, while records handle data storage
    * @return parser-representation of a struct-definition
    */
  def dataStructDef: Parser[DataStructureDefinition] =
    "data" ~> typeDef ~ dataBodyBlock ~ opt("<-" ~> typeDefs) ^^ {
      case typeDefinition ~ dataBlock ~ optionalTypeDefs =>
        DataStructureDefinition(typeDefinition, dataBlock, optionalTypeDefs)
    }

  /** the body of a data structure
    * it's completely optional, because cases exist where only the name is needed
    * one such example is for enums
    * @return a parser-representation of a struct's body
    */
  def dataBodyBlock: Parser[DataBodyBlock] = opt("{" ~> fieldDefs <~ "}") ^^ DataBodyBlock

  /** the fields of a data structure are just the type and the name of the field
    * @return a parser-representation of any number of fields
    */
  def fieldDefs: Parser[FieldDefinitions] = rep(patternVal <~ ";") ^^ FieldDefinitions

  /** a block, unlike the other blocks encountered, is simply a generic one without special rules
    * @return a parser-representation of a block
    */
  def block: Parser[Block] = "{" ~> stmts <~ "}" ^^ {
    statements => Block(Left(statements))
  } | stmt ^^ {
    statement  => Block(Right(statement))
  }

  /** statements are one or more statement */
  def stmts: Parser[Statements] = rep1(stmt) ^^ Statements

  /** statement is an expression or an identifier */
  def stmt: Parser[Statement] = (expr | identifier) <~ ";" ^^ {
    case expression: Expression => Statement(Left(expression))
    case identifier: Identifier => Statement(Right(identifier))
  }

  /** a value-definition is of the form
    * "val literal = expr;" for type-inferred value-definitions
    * "val Type literal = expr;" for explicitly typed value-definitions
    * @return a parser-representation of a value-definition
    */
  def valDef: Parser[ValueDefinition] = "val" ~> (identifier | patternVal) ~ "=" ~ expr <~ ";" ^^ {
    case (identifier: Identifier) ~ "=" ~ expression =>
      ValueDefinition(Left(identifier), expression)
    case (pattern: PatternValue) ~ "=" ~ expression =>
      ValueDefinition(Right(pattern), expression)
  }

  /** function-definitions are represented by either
    * "func identifier(args) = body" for named functions
    * "func(args) = body" for anonymous/unnamed functions
    * @return
    */
  def funDef: Parser[FunctionDefinition] = "func" ~> opt(identifier) ~ "(" ~ arguments ~ ")" ~ "=" ~ block ^^ {
    case optionalId ~ "(" ~ args ~ ")" ~ "=" ~ codeBlock =>
      FunctionDefinition(optionalId, args, codeBlock)
  }

  /** a binary operation is any two expressions separated by an operator that isn't =
    * @return parser-representation of a binary operation
    */
  def binaryOperation: Parser[BinaryOperation] = expr ~ operator ~ expr ^^ {
    case lhs ~ op ~ rhs => BinaryOperation(lhs, op, rhs)
  }

  /** an expression is any statement that returns a value, which is a lot of statements
    * @return a parser-representation of an expression
    */
  def expr: Parser[Expression] = binaryOperation | ifExpr | forCompr | matchExpr | lit | askStmt ^^ {
    case binaryExpression: BinaryOperation => binaryExpression
    case ifExpression => ???
    case forComprehension => ???
    case matchExpression => ???
    case literal => ???
    case ask: AskStatement => ???
  }

  /** the tell statement is the construct that sends a message to an actor
    * example: "tell stack about #push(3)" sends a "#push(3)" message to the actor "stack"
    * @return a parser-representation of the tell statement
    */
  def tellStmt: Parser[TellStatement] = "tell" ~> nonemptyArguments ~ "about" ~ nonemptyArguments <~ ";" ^^ {
    case targets ~ "about" ~ messages => TellStatement(targets, messages)
  }

  /** the ask statement is a parallel to the tell statement
    * rather than being a fire-and-forget it lets the user ask for data to be returned later
    * example: "val a = ask stack about #pop" sends a "#pop" message to "stack",
    * leaving a promised future value in "a"
    * @return a parser-representation of the ask statement
    */
  def askStmt: Parser[AskStatement] = "ask" ~> nonemptyArguments ~ "about" ~ nonemptyArguments <~ ";" ^^ {
    case targets ~ "about" ~ messages => AskStatement(targets, messages)
  }

  /** non-empty arguments are a list of expressions that contain at least one expression */
  def nonemptyArguments: Parser[List[Expression]] = expr ~ rep("," ~> expr) ^^ {
    case expression ~ exprList => expression :: exprList
  }

  /** unlike the non-empty variant, this one is allowed to contain no data */
  def arguments: Parser[List[Expression]] = opt(nonemptyArguments) ^^ {
    case Some(args) => args
    case None       => Nil // Nil = empty list, not null
  }

  def ifExpr = "if" ~> ifBlock ^^ IfExpression
  def ifBlock: Parser[List[IfStatement]] = "{" ~> rep1(ifStmt) <~ "}" ^^ {
    // it's already a list of statements,
    // nothing further needs to be done
    statements => statements
  } | ifStmt ^^ {
    // it's just a single statement, it needs to be wrapped in a List to match
    statement => List(statement)
  }
  def ifStmt: Parser[IfStatement] = expr ~ "then" ~ expr <~ ";" ^^ {
    case boolExpr ~ "then" ~ action => IfStatement(boolExpr, action)
  }

  def matchExpr: Parser[MatchExpression] = "match" ~ "(" ~> expr ~ ")" ~ matchBlock ^^ {
    case expr ~ ")" ~ block => MatchExpression(expr, block)
  }
  def matchBlock: Parser[List[MatchStatement]] = "{" ~> rep1(matchStmt) <~ "}" ^^ {
    statements => statements
  } | matchStmt ^^ {
    statement => List(statement)
  }
  def matchStmt: Parser[MatchStatement] = patternVal ~ "then" ~ expr <~ ";" ^^ {
    case pattern ~ "then" ~ expression => MatchStatement(pattern, expression)
  }

  // TODO: Consider a body instead of an expression, for multi-line for-loops
  def forCompr: Parser[ForComprehension] = "for" ~> forBlock ~ expr ^^ {
    case block ~ expression => ForComprehension(block, expression)
  }
  def forBlock: Parser[List[ForStatement]] = "{" ~>  rep1(forStmt) <~ "}" ^^ {
    statements => statements
  } | forStmt ^^ {
    statement => List(statement)
  }
  def forStmt: Parser[ForStatement] = identifier ~ "in" ~ expr ^^ {
    case id ~ "in" ~ expr => ForStatement(id, expr)
  }

  def list = "[" ~> arguments <~ "]" ^^ { _ => ???}

  def lit: Parser[Literal] = stringLiteral | numberLiteral | atom | list ^^ {
    case stringLiteral  => ???
    case numericLiteral => ???
    case atomLiteral    => ???
    case listLiteral    => ???
  }
}