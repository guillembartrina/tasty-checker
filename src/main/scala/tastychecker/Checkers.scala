package tastychecker

import java.nio.file.Path

import tastyquery.Contexts
import tastyquery.Classpaths.Classpath
import tastyquery.jdk.ClasspathLoaders
import tastyquery.Contexts.Context
import tastyquery.Trees.Tree
import tastyquery.Symbols.TermOrTypeSymbol


private class Checker(val checks: List[Check], val filter: Filter = Filter.empty):
  private val compactedChecks = Check.tryCompactChecks(checks)
  def check(tree: Tree)(using Context): List[Problem] =
    compactedChecks.flatMap(_.check(tree)(using filter))


case class Result(entryPath: Path, toplevelSymbol: TermOrTypeSymbol, problem: Problem)

class TASTyChecker(val checks: List[Check], val filter: Filter = Filter.empty):
  private val checker = Checker(checks, filter)
  def check(targetPaths: List[Path], extraPaths: List[Path] = List.empty[Path]): List[Result] =
    val targetClasspath = ClasspathLoaders.read(targetPaths)
    val extraClasspath = ClasspathLoaders.read(extraPaths)
    val context = Contexts.init(targetClasspath ++ extraClasspath)
    for
      (entry, entryPath) <- targetClasspath.entries.toList.zip(targetPaths)
      symbol <- context.findSymbolsByClasspathEntry(entry).toList
      symbolTree <- symbol.tree(using context).toList  //For java symbols, otherwise: symbolTree = symbol.tree(using context).get
      problem <- checker.check(symbolTree)(using context)
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
