package tastychecker

import tastyquery.Trees.Tree
import tastyquery.Types.Type

// -------------------------------------------------------

abstract class Problem(val name: String, val message: String, val tree: Tree):
  override def toString(): String = s"$name: $message\n${tree.span} # $tree"

// -------------------------------------------------------

case class NotSubtype(val a: Type, val b: Type, override val tree: Tree)
  extends Problem("NotSubtype", s"[$a] is not subtype of [$b]", tree)
