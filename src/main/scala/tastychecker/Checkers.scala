package tastychecker

import java.nio.file.Path

import tastyquery.Contexts
import tastyquery.Contexts.Context
import tastyquery.Classpaths.Classpath
import tastyquery.jdk.ClasspathLoaders
import tastyquery.Trees.Tree
import tastyquery.Symbols.TermOrTypeSymbol


private class TreeChecker(val checks: List[TreeCheck], val filter: TreeFilter = TreeFilter.empty):
  private val mergedTreeChecks = Check.tryMerge(checks)
  def check(tree: Tree)(using Context): List[Problem] =
    mergedTreeChecks.flatMap(_.check(tree)(using filter))

// ----------

case class Result(entryPath: Path, toplevelSymbol: TermOrTypeSymbol, problem: Problem)

class TASTyChecker(override val checks: List[TreeCheck], override val filter: TreeFilter = TreeFilter.empty)
  extends TreeChecker(checks, filter):
  def check(targetPaths: List[Path], extraPaths: List[Path] = List.empty[Path]): List[Result] =
    val targetClasspath = ClasspathLoaders.read(targetPaths)
    val extraClasspath = ClasspathLoaders.read(extraPaths)
    val context = Contexts.init(targetClasspath ++ extraClasspath)
    for
      (entry, entryPath) <- targetClasspath.entries.toList.zip(targetPaths)
      symbol <- context.findSymbolsByClasspathEntry(entry).toList
      symbolTree <- symbol.tree(using context).toList  //Java symbols do not have a tree
      problem <- check(symbolTree)(using context)
    yield
      Result(entryPath, symbol, problem)

object TASTyChecker:
  def printResults(results: List[Result]): Unit =
    println("--------------------------------------------------------------------------------")
    if results.isEmpty
    then println("[NO PROBLEMS FOUND]")
    else
      println("PROBLEMS:")
      for (entryPath, rest) <- results.groupBy(_.entryPath) do
        println("--------------------------------------------------------------------------------")
        println("Entry path: " + entryPath.toAbsolutePath.toString)
        for (toplevelSymbol, rest) <- rest.groupBy(_.toplevelSymbol) do
          println("----------------------------------------")
          println("Symbol: " + toplevelSymbol.name.toString)
          for result <- rest do
            println("--------------------")
            println(result.problem)
    println("--------------------------------------------------------------------------------")
