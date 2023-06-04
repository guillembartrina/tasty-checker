package auxiliar


object Constructs {
  // Basic
  val any: Any = 0: Any
  val anyVal: AnyVal = 0: AnyVal
  val boolean: Boolean = false: Boolean
  val booleanX: boolean.type = boolean
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
  //def methodBooleanToDependentResult(par: Boolean): par.type = par

  trait TraitWithSAMBooleanToBoolean:
    def sam(par: Boolean): Boolean
  trait TraitWithSAMWithDependentResult:
    def sam(par: Boolean): par.type
  type PartialFunctionBooleanToBoolean = PartialFunction[Boolean, Boolean]

  val booleanAndString: Boolean & String = ???
  val booleanOrString: Boolean | String = ???

  class BP extends Product:
    def _1: Boolean = false
    def canEqual(that: Any): Boolean = ???
    def productArity: Int = ???
    def productElement(n: Int): Any = ???

  class SM:
    def get: Boolean = false
    def isEmpty: Boolean = false

  class NBA:
    def _1: Boolean = false
    def _2: Boolean = false

  class NB:
    def get: NBA = new NBA
    def isEmpty: Boolean = false

  object Unapply:
    def unapplyProduct1Boolean(x: Boolean): BP = new BP
    def unapplyProduct1BooleanImplicits(x: Boolean)(y: Boolean): BP = new BP
    def unapplySingleMatch(x: Boolean): SM = new SM
    def unapplyNameBased(x: Boolean): NB = new NB

  val unapplyProduct1BooleanRef = Unapply.unapplyProduct1Boolean(false)
  val unapplyProduct1BooleanImplicitsRef = Unapply.unapplyProduct1BooleanImplicits(false)(false)
  val unapplySingleMatchRef = Unapply.unapplySingleMatch(false)
  val unapplyNameBasedRef = Unapply.unapplyNameBased(false)

  class ClassWithTypeMemberInt { type T = Int }
  class ClassWithTypeMemberZero { type T = String }

  val valDef: Unit = ()
  def defDef[TP](param: Unit): Unit = ()
  class classDef
  type typeMember = Unit
  //type typeLambdaTree = [X] =>> X
  type refinedTypeTree = { def C: Boolean }
  val match0 = (0: Any) match
    case t @ _ => 0
    case _: List[t] => 1
  val typeMatch: (Int match
    case List[t] => t
    case Int => Boolean
  ) = false
}
