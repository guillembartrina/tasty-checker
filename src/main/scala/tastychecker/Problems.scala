package tastychecker

import tastyquery.Symbols.{TermSymbol, TypeSymbol}
import tastyquery.Trees.Tree
import tastyquery.Types.{Type, TypeBounds}


abstract class Problem(val message: String, val tree: Tree):
  override def toString(): String = s"${getClass.getSimpleName}: $message\n# $tree"


case class NotConformsType(val a: Type, val b: Type, override val tree: Tree)
  extends Problem(s"[$a] does not conform to [$b]", tree)

case class NotMatchesType(val a: Type, val b: Type, override val tree: Tree)
  extends Problem(s"[$a] does not match [$b]", tree)

case class NotConformsBounds(val a: Type | TypeBounds, val b: TypeBounds, override val tree: Tree)
  extends Problem(s"[$a] does not conform to bounds [$b]", tree)

case class NotOverrides(val a: TermSymbol, val b: TermSymbol, override val tree: Tree)
  extends Problem(s"[$a] does not override [$b]", tree)

case class NotInScope(val a: TermSymbol | TypeSymbol, override val tree: Tree)
  extends Problem(s"[$a] is not in scope", tree)
