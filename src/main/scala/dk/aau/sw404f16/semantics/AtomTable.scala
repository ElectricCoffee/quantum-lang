package dk.aau.sw404f16.semantics

import dk.aau.sw404f16.syntax.Expression

import scala.collection.mutable

/**
  * Created by coffee on 5/9/16.
  */
object AtomTable {
  val table = mutable.Map[String, List[Expression]]
}
