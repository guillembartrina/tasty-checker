package dummy

object Auxiliar {
  def funStringToString(par: String): String = par
  def funByNameStringToString(par: => String): String = par

  val x = funStringToString("xx")

  def funStringToAnyRef(par: String): AnyRef = par
  def funStringToNull(par: String): Null = null
  def funStringToInt(par: String): Int = 0

  trait StringToString:
    def stringToString(par: String): String
}