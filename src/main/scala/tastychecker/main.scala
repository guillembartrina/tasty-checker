package tastychecker

import java.nio.file.{Path, Paths, FileSystems}

import tastyquery.*
import tastyquery.Classpaths.*
import tastyquery.Contexts.*
import tastyquery.jdk.ClasspathLoaders

// -------------------------------------------------------

@main def main() = {

  val default: List[Path] = List()

  val target: List[Path] = List()
  val extra: List[Path] = List() ++ default

  TASTyChecker.check(target, extra)
}

// -------------------------------------------------------

object TASTyChecker:
  val checks = List(LSP, LSPComp, LSPStat)

  private def printProblems(problems: List[Problem]): Unit =
    println("--------------------------------------------------")
    if problems.isEmpty
    then println("NO PROBLEMS FOUND")
    else
      println("PROBLEMS:")
      for p <- problems do
        println("--------------------------------------------------")
        println(p)
    println("--------------------------------------------------")

  def check(target: Classpath, extra: Classpath): Unit =
    val context = Contexts.init(target ++ extra)
    given Context = context
    val checker = Checker(checks)
    for entry <- target.entries do
      val symbols = context.findSymbolsByClasspathEntry(entry)
      checker.check(symbols.map(_.tree.get))
    printProblems(checker.allProblems)

  def check(target: List[Path], extra: List[Path]): Unit =
    val targetClasspath = ClasspathLoaders.read(target)
    val extraClasspath = ClasspathLoaders.read(extra)
    check(targetClasspath, extraClasspath)
