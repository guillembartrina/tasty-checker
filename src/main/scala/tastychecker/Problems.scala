package tastychecker

import tastyquery.Symbols.{TermSymbol, TypeSymbol}
import tastyquery.Trees.Tree
import tastyquery.Types.{Type, TypeBounds}


abstract class Problem(val message: String, val obj: Any):
  override def toString(): String = s"${getClass.getSimpleName}: $message\n# $obj"


case class NotConformsType(val a: Type, val b: Type, override val obj: Tree)
  extends Problem(s"[$a] does not conform to [$b]", obj)

case class NotMatchesType(val a: Type, val b: Type, override val obj: Tree)
  extends Problem(s"[$a] does not match [$b]", obj)

case class NotConformsBounds(val a: Type | TypeBounds, val b: TypeBounds, override val obj: Tree | Type)
  extends Problem(s"[$a] does not conform to bounds [$b]", obj)

case class NotOverrides(val a: TermSymbol, val b: TermSymbol, override val obj: Tree)
  extends Problem(s"[$a] does not override [$b]", obj)

case class NotInScope(val a: TermSymbol | TypeSymbol, override val obj: Type)
  extends Problem(s"[$a] is not in scope", obj)
