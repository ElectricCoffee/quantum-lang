package dk.aau.sw404f16.semantics
import scala.collection.mutable
/**
  * Created by coffee on 4/14/16.
  */
object SymbolTable {
  type SymTab = mutable.Map[String, String]
  var contents: SymTab = mutable.Map.empty
}
