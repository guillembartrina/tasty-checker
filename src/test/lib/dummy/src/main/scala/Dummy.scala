package dummy

import scala.annotation.targetName


object ExprTypeConformance {

  //Template
  val template1 = new AnyRef { val x = 0; import scala.Predef; }
  val template2 = new AnyRef { type T = String; def x(x: Int) = 0; }
  val template3 = new AnyRef {}
  val template4 = new AnyRef { false }
  val template5 = new AnyRef { (); }
  val template6 = {
    abstract class Template:
      val x = ()
      def y(x: Int): false
  }

  type CaseDef = Boolean
  val caseDef0: Boolean = false
  //CaseDef
  val caseDef = {
    0 match
      case 0 if (true: Boolean & true) => 0
      case 1 if (true: CaseDef) => 1
      //case 2 if (caseDef0: caseDef0.type) => 2  //BREAKING
  }

  //ValDef
  val valDef1: AnyVal = false
  val valDef2: Null = null
  val valDef3: valDef2.type = null
  val valDef4: Option[false] = Some(false)
  
  //DefDef
  def defDef1(x: Int): AnyVal = x
  def defDef2[T](x: T): T = x
  def defDef3[T](x: T): x.type = x
  def defDef4(x: Int): valDef2.type = null

  //Apply
  def applyAux1(x: Int, y: Int, a: Int): Int = x
  val apply1 = applyAux1(y = 1, a = 2, x = 0)
  val apply11 = applyAux1(0, 1, a = 1)

  val apply12 = defDef2("string")

  abstract class SAM:
    def sam(x: Int): Int

  def applyAux2(x: SAM): Int = 0
  def applyAux21(x: Int): Int = 0
  val apply2 = applyAux2((x: Int) => x)
  val apply21 = applyAux2(applyAux21)

  def applyAux3[T](x: T): T = x
  val apply3 = applyAux3(0)

  class Apply4:
    def apply(x: Int): Int = x
    def anohter(x: Int): Int = x
  val apply4 = (new Apply4)(0)
  val apply41 = (new Apply4).anohter(0)

  def applyAux5(x: Int*): Int = 0
  val apply5 = applyAux5(Seq(0, 1, 2)*)

  def applyAux6(x: Int, y: Int = 0): Int = 0
  val apply6 = applyAux6(0)
  val apply61 = applyAux6(0, 1)

  class Apply7:
    type T = Int

  class Apply71:
    type Z
  class Apply72 extends Apply71:
    type Z = Int

  def applyAux7(x: Any): x.type = x
  val apply7 = applyAux7(0)
  def applyAux71(x: Apply7): x.T = 0
  val apply71 = applyAux71(new Apply7)
  def applyAux72(x: Any, y: x.type): Int = 0
  val apply72 = applyAux72(0, 0)
  def applyAux73(x: Apply7, y: x.T): Int = y
  val apply73 = applyAux73(new Apply7, 8)
  def applyAux74(x: Apply71, y: x.Z): Unit = ()
  val apply74 = applyAux74(new Apply72, 8)

  val applyAux81 = (x: Int) => x
  val apply81 = applyAux81(0)
  val applyAux82 = [T] => (x: T) => x
  val apply82 = applyAux82(0)
  val apply821 = applyAux82[Int](0)
  val applyAux83 = [T <: Apply71] => (x: T, y: x.Z) => x
  val apply83 = applyAux82(new Apply72, 0)
  
  given "string" = "string"
  def applyAux9(x: Int)(using y: String): Int = y.length
  val apply9 = applyAux9(0)
  def applyAux91(x: String)(using y: x.type): Int = y.length
  val apply91 = applyAux91("string")

  val applyAux10 = new Function2[Int, Int, Boolean] {
    def apply(v1: Int, v2: Int): Boolean = false
  }
  val apply10 = applyAux10(0, 1)

  def applyAux110(x: Int)(y: Int): Int = x
  val apply110 = applyAux110(0)(0)

  //Assign
  var assign1 = 0
  val assignTmp1 = { assign1 = 1 }

  object Assign {
    var assign = 0
  }
  val assignTmp2 = { Assign.assign = 1 }

  class Assign2 {
    def assign2 = 0
    def assign2_=(x: Int): Unit = ()
    def update(z: Int): Unit = () 
    def update(x: Int, y: Int, z: Int): Unit = () 
    assign2 = 0
  }
  val assign3 = new Assign2
  val assignTmp3 = { assign3.assign2 = 0 }
  val assignTmp31 = { assign3(0, 0) = 0 }
  val assignTmp32 = { assign3() = 0 }

  //Block
  val block1 = {}
  val block2 = { 0 }
  val block3 = { 0; false }
  val block4 = { val y = 0; 0; false }
  val block5 = {
    type T = String
    def x(x: Int): 0 = 0
  }
  val block6 = {
    class Block
  }

  val block7 = { valDef2 }
  val block8 = {
    val inner: 0 = 0
    inner 
  }
  val blockAux1: 0 = 0
  val block9 = {
    val inner: blockAux1.type = blockAux1
    inner 
  }

  val block10 = {
    class Inner
    new Inner()
  }

  class Outer
  val block11 = {
    class Inner
    new Inner(): Inner | Outer
  }

  //If
  val if1 = if true then 0 else 0
  val if2 = if false: false & AnyVal then 0 else 0

  //InlineIf
  inline def inlineIfAux(x: Int): Int =
    inline if x == 0 then 0 else 1
  val inlineIf = inlineIfAux(0)
  val inlineIf1 = inlineIfAux(1)

  //InlineMatch
  inline def inlineMatchAux(x: Int): Int =
    inline x match
      case 0 => 0
      case 1 => 1
  val inlineMatch = inlineMatchAux(0)

  //Inlined
  inline def inlinedAux1(x: Int) =
    println(x)
    println(x+1)
  val inlined1 = inlinedAux1(0)

  //Lambda
  val lambda1: Int => Int = _ * 2
  val lambda2 = (x: Int) => x
  val lambda21 = (x: Int) => (y: Boolean) => ()
  trait Lambda:
    def sam(x: Int): Int
  val lambda3: Lambda = x => x
  val lambda31: Lambda = (x: AnyVal) => 0: 0
  trait Lambda2[T]:
    def sam(x: T): T
  val lambda4: Lambda2[Int] = (x: Int) => x
  trait Lambda3:
    def sam(x: Int): x.type
  val lambda5: Lambda3 = (x: Int) => x
  val lambda7: PartialFunction[Int, Int] = x => x
  val lambda8: PartialFunction[Int, Int] = { case 0 => 0 }
  
  val lambda9 = [T] => (x: T) => x  // Not a lambda!

  //val lambda10: { def sam(x: Int): Int } = (x: Int) => 0

  //Match
  val match1 = (0: Int) match
    case x: 0 => 0
    case y: 1 => "hello"
  val match2 = "string" match
    case null => 0
    case x => false
  val match3 = (false: Any) match
    case _: String => 0
  
  //Return
  def return1(x: Int): Int = {
    if true then return 0
    if true then return (0: Int & 0)
    0
  }
  def return2(x: Int): AnyVal = {
    if true then return 0
    if true then return
    false
  }
  def return3(x: Int): Unit = {
    if true then return
    if true then return ()
    ()
  }
  //val return4 = (x: Int) => { return 0 }

  //SeqLiteral
  val seqLiteral1 = Seq()
  val seqLiteral2 = Seq(0)
  val seqLiteral3 = Seq(0, false)
  val seqLiteral4 = Seq(null)
  val seqLiteral5 = Seq[valDef2.type](valDef2, valDef2)
  val seqLiteral6 = Seq[Any](0, false)

  //Throw
  val throw1 = throw NullPointerException()
  val throw2 = throw Exception()
  val throw3 = throw Exception(): Throwable
  val throw4 = throw null

  //Try
  val try1 = try 0
  val try2 = try 0 finally 0
  val try3 = try {throw Exception() } finally ()
  val try4 = try 0 catch case e: Exception => false
  val try5: 0 | false = try 0 catch case e: Exception => false
  val try6 = try 0 catch case e: Exception => null finally ()
  val try7 = try 0 catch (null: PartialFunction[Throwable, Int])
  val try8 = try 0 catch (x: Throwable) => 0

  //Typed
  val typed1: AnyVal = 1: Int
  val typed2: AnyRef = null: String
  val typedAux3 = 0
  val typed3 = typedAux3: typedAux3.type
  val typed4 = null: valDef2.type
  val typed5 = true: true & Boolean
  val typed6 = "string": "string"
  case class Typed(val x: Int)
  val typed7 = (new Typed(0)): Typed

  //While
  val while1 = while true do ()
  val while2 = while true do "string"
  val while3 = while true do null
  val while4 = while true do { (); 0 }
  val while5 = while true do (): Unit & AnyVal
  val while6 = while true do { def x(): Unit = (); x() }
  val while7 = while true do { (): AnyVal }

}

object MatchingTypeCorrectness {
  val match0 = (0: Any) match
    case x => x
    case _ => 0

  final val M = 12
  val match1 = (0: Any) match
    case 0 => 0
    case false => 1
    case "string" => 2
    case M => 4

  case class Person(age: Int, name: String)
  val match2 = (0: Any) match
    case Some(x) => x
    case Person(age, "name") => age
    case Some(Some(y: Int)) => y
    case x @ Some(y @ 0) => 3
  
  val match3 = (0: Any) match
    case x @ 12 => x
    case x @ (0: Int) => x
    case x @ Some(y) => x

  val match4 = (0: Any) match
    case x: 0 => x
    case x: Some[Int] => x
    case Some(x: Int) => x
    case Person(age, y: String) => age

  val match41 = (0: AnyVal) match
    case x: Some[Int] => x

  val match5 = (0: Any) match
    case x: 0 => x
    case x: Some[Int] => x
    case Some(x: Int) => x

  case class Rep(nums: Int*)
  val match6 = (0: Any) match
    case Rep(0, 0) => 0
    case Rep(n, ns*) => 1
    case Rep(_*) => 2

  val match7 = (0: Any) match
    case 0 | 1 | 2 => 0
    case _: Int | _: String => 1
    case Person(_, _) | M => 2
    case Some(_) | Left(_) => 3

  class Test(val x: Int)
  object Test:
    def unapply(x: Test)(using Int): Option[x.type] = Some(x)

  given Int = 0
  val match8 = (0: Any) match
    case Test(x): Test => x

  val match9 = 0 match
    case t @ y: Int => t
    case (t @ (x: Int)): Int => 
      
  val match10 = (0: Any) match
    case Some(_: Int | _: Double) => 0
  
}

object TypeBoundsConformance {
  //TypeApply
  def tpAux01[Boolean <: AnyVal](x: Boolean): Boolean = x
  val tp01 = tpAux01(true)
  def tpAux02[List[Int]](x: Boolean): Boolean = x
  val tp02 = tpAux02(true)
  def tpAux03[List[Int <: AnyVal]](x: Boolean): Boolean = x
  val tp03 = tpAux03(true)
  def tpAux04[Option[_]](x: Boolean): Boolean = x
  val tp04 = tpAux04(true)
  def tpAux05[List](x: Boolean): Boolean = x
  val tp05 = tpAux05(false)

  def tpAux1[T <: Int](x: T): T = x
  val tp1 = tpAux1(0)
  val tpAuxAux2 = false
  def tpAux2[T <: tpAuxAux2.type](x: T): T = x
  val tp2 = tpAux2(tpAuxAux2)
  def tpAux3[T <: 1](x: T): T = x
  val tp3 = tpAux3(1)
  
  def tpAux4[T](x: Int) = 0
  val tp4 = tpAux4(0)
  
  def tpAux5[T <: List[_]](x: T): Boolean = x.isEmpty
  val tp5 = tpAux5(Nil)
  val tp51 = tpAux5(List(1, 2))
  
  val tpAux6 = [T] => (x: T) => x
  val tp6 = tpAux6(0)
  
  def tpAux7[T <: Int](x: List[T]): Unit = ()
  def tp7[T <: Int](x: List[T]): Unit = tpAux7(x)
  def tp71[T <: Int](x: List[_ <: T]): Unit = tpAux7(x)
  
  def tpAux8[X <: Boolean](x: X): Boolean = tpAux01(x)
  val tp8 = tpAux8(false)
  
  def tpAux9[X <: Option[Map[_, Int]]](x: X): Int = 0
  val tp9 = tpAux9(None)
  
  class TpAux[A]
  def tpAux10[T <: List[?]](x: TpAux[T]): Unit = ()
  def tp10[T <: List[?]](x: TpAux[_ <: T]): Unit = tpAux10(x)

  def tp11 = ([X] => (x: X) => [Y] => (x: Y) => false)

  //AppliedTypeTree
  val tl1: (([Int <: AnyVal] =>> Int)[Int]) = 0
  val tl2: (([X <: Boolean] =>> X)[false]) = false
  val tl3: (([List[_]] =>> Boolean)[List]) = false
  val tl4: (([X] =>> [Y <: Int] =>> Boolean)[Any][0]) = false
  val tl5: (([A, List[Boolean]] =>> Boolean)[Int, List]) = false
  val tl6: (([X <: Any] =>> Option[List[X]])[Boolean]) = None
  val tl7: (([Option[_]] =>> Int)[Option]) = 0

  type X[Y] = List[Int]
  val yl5: X[Int] = Nil
}

object TypeMemberOverridingBoundsConformance {
  class SH1 { type T <: Int }
  class SH2 extends SH1 { type T <: 0 }
  class SH3 extends SH2 { type T = 0 }

  trait DHA { type T <: DHA }
  trait DHB extends DHA { type T <: DHB }
  trait DHC extends DHA { type T <: DHC }
  trait DHD extends DHB, DHC { type T <: DHD }

  class T1 { type T <: Set[_] }
  class T2 extends T1 { type T <: Null }
  
  class ASF1 { type M; type N <: M }
  class ASF2 extends ASF1 { type M = Int; type N = 0 }
}

object MemberOverridingTypeConformance {
  trait MTC:
    type T <: Int
    def meth(x: Int): Int

  class MTC1 extends MTC:
    def meth(x: Int): Int = 0

  class MTC11 extends MTC:
    def meth(x: Int): 0 = 0

  class MTC12 extends MTC:
    type T = Int
    def meth(x: T): Int = 0

  abstract class X1:
    type M
    type N <: M
    def meth(x: N): Int = 0
    def meth2(x: Int): Int = 0
    val mem: N
    val mem2: N

  class X2 extends X1:
    type M = Int
    type N = 0
    override def meth(x: 0): Int = 0
    override def meth2(x: Int): 0 = 0
    val mem: N = 0
    val mem2: 0 = 0


  abstract class M1:
    val mem: Int = 0
    val mem2: String = "string"
    def str: String
    def meth[A](x: A): x.type
    def meth2[A >: List[_]]: A
    def meth3(x: Int): Int = 0

  class M2 extends M1:
    override val mem: 0 = 0
    override val mem2: "string" = "string"
    val str: String = "string"
    def meth[B](x: B): x.type = x
    def meth2[B >: List[_]]: B = List.empty[B]

  class M3 extends M2:
    override def meth3(x: Int): Int = 12

  class A1 extends Object:
    override def toString(): String = "string"
    def meth(): Int = 0

  class A2 extends A1:
    override def toString: String = "string"
    override def meth(): Int = 0

  class A3 extends A2:
    override val toString: String = "string"

  trait BA { def run: Unit }
  class BB extends Runnable with BA { override def run(): Unit = () }

  class C1 extends Object:
    override def toString: String = "string"
    def meth(): Int = 0

  class C2 extends C1:
    override def toString(): String = "string"
    override def meth(): Int = 0

  class D1 extends Object:
    override val toString: String = "string"

  class E1 extends Object:
    override def toString(): String = "string"

  abstract class F1:
    def meth(x: List[Int]): Int
    @targetName("meth2")
    def meth(x: List[Boolean]): Int = 0
    @targetName("y")
    def x(x: Int): Int

  class F2 extends F1:
    @targetName("meth")
    override def meth(x: List[Int]): Int = 0
    @targetName("y")
    override def x(x: Int): Int = 0
}

object MemberErasureOverridance {
  abstract class MOR0:
    val m: Int = 0
    def me[A](x: A): A = x
    def sn(x: List[Int]): Unit
    @targetName("sn2") def sn(x: List[Double]): Unit = ()
  abstract class MOR1 extends MOR0:
    val mem: Int
    def meth(x: List[Int]): Int = 0
    def meth2[Boolean](x: Int): Int = 0
    override def sn(x: List[Int]): Unit = ()
  class MOR2 extends MOR1:
    override val m: Int = 0
    override val mem: 0 = 0
    override def meth(x: List[Int]): Int = 0
    override def meth2[AnyVal](x: Int): Int = 0
    override def me[B](x: B): B = x
}

object LocalReferencesScoping {
  val scAux1 = 0
  val sc1 = scAux1  //not the case

  case class SC0(x: Int)
  val scAux2 = SC0(0)
  val sc2 = scAux2.x  //not the case

  val sc3 = {
    val innerVal = 0
    val localRef = innerVal
    ()
  }

  val sc31 = {
    def innerDef(x: Int) = 0
    val localRef = innerDef(0)
    ()
  }

  val sc32 = {
    val innerVal = 0
    {
      val innerRef = innerVal
    }
    {
      val innerInnerVal = 0
      val innerRef = innerVal
    }
  }

  trait SC1:
    type T
  class SC11 extends SC1:
    type T = Int

  def scAux4(x: SC1, y: x.T, z: x.type): SC1 = x
  val scAux41 = new SC11
  val sc4 = scAux4(scAux41, 0, scAux41)

  val sc5 = (0: Any) match
    case Some(x) => x
    case x => x

  val sc6 = {
    class A
    val x: A = new A()
  }

  val sc61 = {
    type T = Int
    val x: T = 0
  }

  def scAux7[T](x: T): T = x
  val sc7 = scAux7(false)


  val sc8: (List[Int] match
    case List[t] => t) = 0

  def sc9[A]: (List[Int] match
    case List[t] => t
    case A => A  
  ) = 0

  val sc10 = (List(0): Any) match
    case a: List[t] => a.head
    //case b: Option[?] => b

  type SC12[X] = X match {
    case List[t] => t
  }
  def sc12[X](x: X): SC12[X] = x match
    case is: List[l] => is.head

  import scala.quoted.*
  def sc13(using quotes: Quotes) =
    Type.of[Any] match
      case '[Option[a]] => '{ () }
}

// -----------

object Dummy {
  def main(args: Array[String]): Unit =
    println("Hello, World!")

  def g(xs: Seq[Int]): Int = xs match
    case List(elems: _*) => 0
    case _               => 1
}
