package dk.aau.sw404f16.semantics

/**
  * Created by coffee on 4/24/16.
  */

// "companion object" allows for static methods
object TypeInfo {
  val any        = new TypeInfo("Any", Nil, null) // special case. Any is the equivalent of "Object" in C#
  val number     = new TypeInfo("Number")
  val string     = new TypeInfo("String")
  val boolean    = new TypeInfo("Boolean")
  val unit       = new TypeInfo("Unit") // equivalent to void
  val list       = list(any)
  val dictionary = dictionary(any, any)
  val function   = function(unit) // void function with no arguments

  def list(valueType: TypeInfo) = new TypeInfo("List", List(valueType))

  def dictionary(keyType: TypeInfo, valueType: TypeInfo) =
    new TypeInfo("Dictionary", List(keyType, valueType))

  def dictionary(keyType: String, valueType: String) =
    dictionary(new TypeInfo(keyType), new TypeInfo(valueType))

  def function(retType: TypeInfo, argTypes: List[TypeInfo]) =
    new TypeInfo("Function", retType :: argTypes)

  def function(retType: String, argTypes: List[String]) =
    function(new TypeInfo(retType), argTypes.map(new TypeInfo(_)))

  def function(retType: TypeInfo) = function(retType, Nil)
  def function(retType: String)   = function(retType, Nil)
}

class TypeInfo(val concreteType: String, val typeArguments: List[TypeInfo], val superType: TypeInfo) {
  // constructor overloads. Default constructor must have all input arguments
  def this(concreteType: String, typeArguments: List[TypeInfo]) = this(concreteType, typeArguments, TypeInfo.any)
  def this(concreteType: String) = this(concreteType, Nil)

  /** returns true if both concrete types match */
  def <=> (that: TypeInfo): Boolean  = this.concreteType == that.concreteType
  /** returns false if both concrete types match */
  def <!=>(that: TypeInfo): Boolean  = !(this <=> that)

  /** returns true if both super types match */
  def <==> (that: TypeInfo): Boolean = this.superType <=> that.superType
  /** returns false if both super types match */
  def <!==>(that: TypeInfo): Boolean = !(this <==> that)

  /** returns true if the super type matches the concrete type (reverse of <=^>) */
  def <^=> (that: TypeInfo): Boolean = this.superType.concreteType == that.concreteType
  /** returns false if the super type matches the concrete type (reverse of <!=^>) */
  def <!^=>(that: TypeInfo): Boolean = !(this <^=> that)

  /** returns true if the concrete type matches the super type (reverse of <^=>) */
  def <=^> (that: TypeInfo): Boolean = this.concreteType == that.superType.concreteType
  /** returns false if the concrete type matches the super type (reverse of <!^=>) */
  def <!=^>(that: TypeInfo): Boolean = !(this <=^> that)
}
