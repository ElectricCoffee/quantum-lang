package dk.aau.sw404f16.semantics

/**
  * Created by coffee on 4/24/16.
  */

// "companion object" allows for static methods
object TypeInfo {
  val any     = new TypeInfo("Any", Nil, null) // special case. Any is the equivalent of "Object" in C#
  val number  = new TypeInfo("Number")
  val string  = new TypeInfo("String")
  val boolean = new TypeInfo("Boolean")
  val unit    = new TypeInfo("Unit") // equivalent to void
  def list(valueType: TypeInfo) = new TypeInfo("List", List(valueType))
  def dictionary(keyType: TypeInfo, valueType: TypeInfo) = ("Dictionary", List(keyType, valueType))
}

class TypeInfo(val concreteType: String, val typeArguments: List[TypeInfo], val superType: TypeInfo) {
  // constructor overloads. Default constructor must have all input arguments
  def this(concreteType: String, typeArguments: List[TypeInfo]) = this(concreteType, typeArguments, TypeInfo.any)
  def this(concreteType: String) = this(concreteType, Nil)
}