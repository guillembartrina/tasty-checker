package auxiliar

object Constructs {
  // Basic
  val any: Any = 0: Any
  val anyVal: AnyVal = 0: AnyVal
  val boolean: Boolean = false: Boolean
  val _false: false = false
  val string: String = "string": String

  val anyRef: AnyRef = null: AnyRef
  val throwable: Throwable = null: Throwable
  val _null: Null = null: Null

  val unit: Unit = (): Unit

  val seqBoolean: Seq[Boolean] = Seq(true): Seq[Boolean]

  def methodBooleanToBoolean(par: Boolean): Boolean = par
  def methodByNameBooleanToBoolean(par: => Boolean): Boolean = par
  def methodBooleanAndDependentArgumentToBoolean(par1: Boolean, par2: par1.type): Boolean = par1

  trait TraitWithSAMBooleanToBoolean:
    def sam(par: Boolean): Boolean
  trait TraitWithSAMWithDependentResult:
    def sam(par: Boolean): par.type
  type PartialFunctionBooleanToBoolean = PartialFunction[Boolean, Boolean]
}
