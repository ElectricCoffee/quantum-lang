package dk.aau.sw404f16.semantics

import dk.aau.sw404f16.syntax.{Atom, Expression}

import scala.collection.mutable

/**
  * Created by coffee on 5/9/16.
  */
object AtomTable {
  private type AtomDict = mutable.Map[String, List[Expression]]
  val table: AtomDict = mutable.Map.empty

  def addAtom(atom: String, args: List[Expression]): Unit = {
    if (!table.contains(atom))
      table += atom -> args

    // TODO: decide what this method should return
  }

  def addAtom(atom: Atom, args: List[Expression]): Unit = addAtom(atom.data, args)

  def addAtom(atom: String): Unit = addAtom(atom, Nil)

  def addAtom(atom: Atom): Unit = addAtom(atom, Nil)
}
