package auxiliar

object Constructs {
  // Basic
  val any: Any = 0: Any
  val anyVal: AnyVal = 0: AnyVal
  val boolean: Boolean = false: Boolean
  val _false: false = false
  val string: String = "string": String

  def methodBooleanToBoolean(par: Boolean): Boolean = par

  val anyRef: AnyRef = null: AnyRef
  val throwable: Throwable = null: Throwable
  val _null: Null = null: Null

  val unit: Unit = (): Unit

  val seqBoolean: Seq[Boolean] = Seq(true): Seq[Boolean]

  def methodByNameBooleanToBoolean(par: => Boolean): Boolean = par

  val funBooleanToBoolean = methodBooleanToBoolean(true)
  val funByNameBooleanToBoolean = methodByNameBooleanToBoolean(true)

  /*
  def funStringToString(par: String): String = par
  def funByNameStringToString(par: => String): String = par

  val x = funStringToString("xx")

  def funStringToAnyRef(par: String): AnyRef = par
  def funStringToNull(par: String): Null = null
  def funStringToInt(par: String): Int = 0

  trait StringToString:
    def stringToString(par: String): String
  */
}
