package tastychecker

import tastyquery.Contexts.*
import tastyquery.Trees.*
import tastyquery.Types.*

// -------------------------------------------------------

abstract class Check(val name: String):
  def check(tree: Tree)(using Context): List[Problem]

