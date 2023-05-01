package tastychecker

import tastyquery.Trees.Tree
import tastyquery.Types.*

abstract class Problem(val message: String, val tree: Tree):
  override def toString(): String = s"${getClass.getSimpleName}: $message\n# $tree"


case class NotConformsType(val a: Type, val b: Type, override val tree: Tree)
  extends Problem(s"[$a] does not conform to [$b]", tree)

case class NotMatchesType(val a: Type, val b: Type, override val tree: Tree)
  extends Problem(s"[$a] does not match [$b]", tree)

case class NotConformsBounds(val a: Type, val b: TypeBounds, override val tree: Tree)
  extends Problem(s"[$a] does not conform to bounds [$b]", tree)
