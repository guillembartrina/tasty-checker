package auxiliar

object PotentialBugs {
  val pb1: String = "string"
  val pb2: pb1.type = null

  // Breaking
  // val pb3: Boolean = false
  //val caseDef = {
  //  0 match
  //    case 0 if (pb3: pb3.type) => 2
  //}

  val pb3 = [T] => (x: T) => x
  val pb4 = pb3(0)
}