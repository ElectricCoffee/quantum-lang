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

  /** Creates a "List" type instance.
    * @param valueType the type of the list's elements
    * @return a new TypeInfo instance for a list
    */
  def list(valueType: TypeInfo): TypeInfo = new TypeInfo("List", List(valueType))

  /** Creates a "List" type instance.
    * Note that this function doesn't allow any choice of super-type.
    * @param valueType the type of the list's elements as a string
    * @return a new TypeInfo instance for a list
    */
  def list(valueType: String): TypeInfo = list(new TypeInfo(valueType))

  /** Creates a "Dictionary" type instance.
    * @param keyType the type of the dictionary's keys
    * @param valueType the type of the dictionary's values
    * @return a new TypeInfo instance for a dictionary
    */
  def dictionary(keyType: TypeInfo, valueType: TypeInfo): TypeInfo =
    new TypeInfo("Dictionary", List(keyType, valueType))

  /** Creates a "Dictionary" type instance.
    * Note that this function doesn't allow any choice of super-type.
    * @param keyType the type of the dictionary's keys as a string
    * @param valueType the type of the dictionary's values as a string
    * @return a new TypeInfo instance for a dictionary
    */
  def dictionary(keyType: String, valueType: String): TypeInfo =
    dictionary(new TypeInfo(keyType), new TypeInfo(valueType))

  /** Creates a "Function" type instance.
    * The list of type arguments has the return type as the first argument always.
    * @param retType the function's return type.
    * @param argTypes the function's argument types.
    * @return a new TypeInfo instance for a function.
    */
  def function(retType: TypeInfo, argTypes: List[TypeInfo]): TypeInfo =
    new TypeInfo("Function", retType :: argTypes)

  /** Creates a "Function" type instance.
    * The list of type arguments has the return type as the first argument always.
    * Note that this version doesn't allow a choice of super-type.
    * If you want to use a super-type other than "Any", use the other version.
    * @param retType the function's return type as a string.
    * @param argTypes the function's argument types as a string.
    * @return a new TypeInfo instance for a function.
    */
  def function(retType: String, argTypes: List[String]): TypeInfo =
    function(new TypeInfo(retType), argTypes.map(new TypeInfo(_)))

  /** Creates a "Function" type instance.
    * This variant doesn't allow function arguments.
    * @param retType The function's return type.
    * @return a new TypeInfo instance for a function.
    */
  def function(retType: TypeInfo): TypeInfo = function(retType, Nil)

  /** Creates a "Function" type instance.
    * This variant doesn't allow function arguments.
    * @param retType The function's return type as a string.
    * @return a new TypeInfo instance for a function.
    */
  def function(retType: String): TypeInfo = function(retType, Nil)
}

class TypeInfo(val concreteType: String, val typeArguments: List[TypeInfo], val superType: List[TypeInfo]) {
  // constructor overloads. Default constructor must have all input arguments
  def this(concreteType: String, typeArguments: List[TypeInfo]) = this(concreteType, typeArguments, List(TypeInfo.any))
  def this(concreteType: String) = this(concreteType, Nil)

  /** returns true if both concrete types match */
  def <=> (that: TypeInfo): Boolean  = this.concreteType == that.concreteType
  /** returns false if both concrete types match */
  def <!=>(that: TypeInfo): Boolean  = !(this <=> that)

  /** returns true if all super types match */
  def <==> (that: TypeInfo): Boolean = (for {
    concrete <- this.superType
    base <- that.superType
  } yield concrete <=> base).forall(b => b)

  /** returns false if both super types match */
  def <!==>(that: TypeInfo): Boolean = !(this <==> that)

  /** returns true if a super type matches the concrete type */
  def <?=>(that: TypeInfo): Boolean = this.superType.exists(_.concreteType == that.concreteType)
  /** returns false if a super type matches the concrete type */
  def <!?=>(that: TypeInfo): Boolean = !(this <?=> that)

  /** returns true if all super types match the concrete type */
  def <^=>(that: TypeInfo): Boolean = this.superType.forall(_.concreteType == that.concreteType)
  /** returns false if all super types match the concrete type */
  def <!^=>(that: TypeInfo): Boolean = !(this <^=> that)

  /** returns true if a concrete type matches the super type */
  def <=?>(that: TypeInfo): Boolean = that.superType.exists(_.concreteType == this.concreteType)
  /** returns false if a concrete type matches the super type */
  def <!=?>(that: TypeInfo): Boolean = !(this <=?> that)

  /** returns true if all concrete types match the super type */
  def <=^>(that: TypeInfo): Boolean = that.superType.exists(_.concreteType == this.concreteType)
  /** returns false if all concrete types match the super type */
  def <!=^>(that: TypeInfo): Boolean = !(this <=^> that)

  def makeSubTypeOf(bases: List[TypeInfo]) = new TypeInfo(this.concreteType, this.typeArguments, bases)
  def makeSubTypeOf(base: TypeInfo): TypeInfo = makeSubTypeOf(List(base))
}
