package tastychecker

import tastyquery.Trees.Tree
import tastyquery.Types.Type

// -------------------------------------------------------

abstract class Problem(val message: String, val tree: Tree):
  override def toString(): String = s"${getClass.getSimpleName}: $message\n# $tree"

// -------------------------------------------------------

case class NotSubtype(val a: Type, val b: Type, override val tree: Tree)
  extends Problem(s"[$a] is not subtype of [$b]", tree)
