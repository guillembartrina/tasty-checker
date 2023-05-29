package auxiliar


object PotentialBugs {
  val pb1: String = "string"
  val pb2: pb1.type = null

  //Breaking
  //val pb3: Boolean = false
  //val caseDef = {
  //  0 match
  //    case 0 if (pb3: pb3.type) => 2
  //}

  val pb3 = [T] => (x: T) => x
  val pb4 = pb3(0)

  val tl7: (([X] =>> Int)[0]) = 0
  val tl4: (([X] =>> [Y <: Int] =>> Boolean)[Any][0]) = false

  def tpAux7[T <: Int](x: List[T]): Unit = ()
  def tp71[T <: Int](x: List[_ <: T]): Unit = tpAux7(x)

  class Apply71:
    type Z
  class Apply72 extends Apply71:
    type Z = Int
  def applyAux74(x: Apply71, y: x.Z): Unit = ()
  val apply74 = applyAux74(new Apply72, 8)
}