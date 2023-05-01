package dummy

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
      //case 2 if (caseDef0: caseDef0.type) => 2
  }

  //ValDef
  val valDef1: AnyVal = false
  val valDef2: Null = null
  //[] val valDef3: valDef2.type = null
  val valDef4: Option[false] = Some(false)
  
  //DefDef
  def defDef1(x: Int): AnyVal = x
  def defDef2[T](x: T): T = x
  def defDef3[T](x: T): x.type = x
  //[] def defDef4(x: Int): valDef2.type = null

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
  //val apply22: SAM = ((x: Int) => (x: Int) => x)(0) why is this incorrect?

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
  //[] val apply72 = applyAux72(0, 0)
  def applyAux73(x: Apply7, y: x.T): Int = y
  val apply73 = applyAux73(new Apply7, 8)
  def applyAux74(x: Apply71, y: x.Z): Unit = ()
  //[] val apply74 = applyAux74(new Apply72, 8)

  val applyAux81 = (x: Int) => x
  val apply81 = applyAux81(0)
  val applyAux82 = [T] => (x: T) => x
  //val apply82 = applyAux82(0)
  //val apply821 = applyAux82[Int](0)
  //val applyAux83 = [T <: Apply71] => (x: T, y: x.Z) => x
  //val apply83 = applyAux82(new Apply72, 0)
  
  given "string" = "string"
  def applyAux9(x: Int)(using y: String): Int = y.length
  val apply9 = applyAux9(0)
  def applyAux91(x: String)(using y: x.type): Int = y.length
  val apply91 = applyAux91("string")

  val applyAux10 = new Function2[Int, Int, Boolean] {
    def apply(v1: Int, v2: Int): Boolean = false
  }
  val apply10 = applyAux10(0, 1)


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
  trait Lambda2[T]:
    def sam(x: T): T
  val lambda31: Lambda2[Int] = (x: Int) => x
  val lambda4: Lambda = (x: AnyVal) => 0
  //val lambda5: Lambda = ((x: Int) => ((x: Int) => x))(0)
  //trait Lambda3:
  //  def sam(x: Int)(y: Boolean): Int
  //val lambda6: Lambda3 = (x: AnyVal) => (y: Boolean) => 0
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
  //[] val typed4 = null: valDef2.type
  val typed5 = true: true & Boolean
  val typed6 = "string": "string"
  case class Typed(val x: Int)
  val typed7 = (new Typed(0)): Typed


  //While
  var while1 = while true do ()
  var while2 = while true do "string"
  var while3 = while true do null
  var while4 = while true do { (); 0 }
  var while5 = while true do (): Unit & AnyVal
  var while6 = while true do { def x(): Unit = (); x() }
  var while7 = while true do { (): AnyVal }

}

object MatchingTypingInvariants {
  val x = 0 match
    case x => false
    case _ => false    
}

object TypeParamBoundsConformance {
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
}

// -----------

object Dummy {
  def main(args: Array[String]): Unit =
    println("Hello, World!")
}
