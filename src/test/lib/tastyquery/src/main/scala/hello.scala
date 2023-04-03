package hellotest

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

trait A
trait B
class C extends A, B

class Person(val x: C)
object Person:
	def unapply(per: Person): Option[B] = Some(per.x)

object Hello {
	def main(args: Array[String]): Unit =
		println("Hello, World!")

	val x: Seq[Int] = Seq()

	val y = (12: Any) match {
    case x @ (_: Boolean | _: String) => x
    case x @ 5 => 0
    case Person(x) => 12
  }

}

// -----------

// Example:

/*


val x = Person(new C()) match
		case x => x
		case _ => 0
*/
