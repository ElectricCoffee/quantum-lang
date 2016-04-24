package dk.aau.sw404f16.semantics

/**
  * Created by coffee on 4/24/16.
  */
class TypeInfo(val concreteType: String, val typeArguments: List[TypeInfo], val superType: Option[TypeInfo]) {
  // constructor overloads. Default constructor must have all input arguments
  def this(concreteType: String, typeArguments: List[TypeInfo]) = this(concreteType, typeArguments, None)
  def this(concreteType: String) = this(concreteType, Nil)
}
