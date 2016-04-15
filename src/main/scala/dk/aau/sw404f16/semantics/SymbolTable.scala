package dk.aau.sw404f16.semantics
import scala.collection.mutable
/**
  * Created by coffee on 4/14/16.
  */
class SymbolTable(scope: String) {
  type SymTab = mutable.Map[String, Either[String, SymbolTable]]
  var contents: SymTab = mutable.Map.empty
}
