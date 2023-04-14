package dummy

/*
class A
class B extends A

val x = (new B(): Any) match
	case _: B => 1
	case _: A => 0
*/

/*
class Temp:
	type A = {
		def isEmpty: Boolean
		def pap: Int
	}
*/

/*
val x = Person(new C()) match
		case x => x
		case _ => 0
*/

trait A
trait B
class C extends A, B

class Person(val x: C)
object Person:
	def unapply(per: Person): Option[B] = Some(per.x)

object LSP {
  val a1 = 12 match {
    case x if ((x%2 == 0): Boolean & AnyVal) => 0
    case _ => 1
  }

  val a2: AnyVal = 14

  def a3: AnyRef = "Duck"

  def t1(x: AnyVal, y: => String): Int = 5
  val a4 = t1(34, "Pato")

  val a5 = (x: Int) => x+1

  trait Printer:
    def print(x: Seq[Int]): Unit

  val a6: Printer = (x: Seq[Any]) => ()
}

object PseudoLSPMatching {
  val y = (12: Any) match {
    case x @ (_: Boolean | _: String) => x
    case x @ 5 => 0
    case 10: Int => 1
    case j @ Person(x: A) => 12
  }
}

// -----------

object Dummy {
  def main(args: Array[String]): Unit =
    println("Hello, World!")
}
