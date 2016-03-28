package dk.aau.sw404f16.lexer

/**
  * Created by coffee on 28/03/16.
  */
object Regexp {
  // all value names are suffixed with "Tok" to avoid possible clashing with built-in keywords
  val aboutTok     = "about".r
  val actorTok     = "actor".r
  val askTok       = "ask".r
  val assignTok    = "=".r
  val atomTok      = """(#[A-Za-z_][\w'-]*)""".r // triple-quote ignores escapes and allows "
  val binLitTok    = "(0b[01]+)".r
  val commaTok     = ",".r
  val dataTok      = "data".r
  val defineTok    = "define".r
  val forTok       = "for".r
  val funcTok      = "func".r
  val hexLitTok    = "(0x[0-9A-Fa-f]+)".r
  val idTok        = """([a-zA-Z_][\w'-]*)""".r
  val ifTok        = "if".r
  val importTok    = "import".r
  val inTok        = "in".r
  val inheritTok   = "<-".r
  val lBraceTok    = """\{""".r
  val lBracketTok  = """\[""".r
  val lParenTok    = """\(""".r
  val matchTok     = "match".r
  val moduleTok    = "module".r
  val numLitTok    = "([0-9]+)".r
  val ofTok        = "of".r
  val operatorTok  = """([^\w'-]+)""".r
  val periodTok    = """\.""".r
  val receiverTok  = "receiver".r
  val rBraceTok    = """\}""".r
  val rBracketTok  = """\]""".r
  val rParenTok    = """\)""".r
  val semicolonTok = ";".r
  val stringLitTok = """(\"(?:\\.|[^"])*\")""".r // fixed with help from Wiktor Stribiżew on StackOverflow
  val tellTok      = "tell".r
  val thenTok      = "then".r
  val valueTok     = "val".r
}
