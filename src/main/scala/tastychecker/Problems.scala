package tastychecker

import tastyquery.Trees.Tree
import tastyquery.Types.Type

// -------------------------------------------------------

abstract class Problem(val name: String, val message: String, val tree: Tree):
  override def toString(): String = s"$name: $message\n${tree.span} # $tree"
