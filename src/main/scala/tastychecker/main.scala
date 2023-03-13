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

  val problems = TASTyChecker(Check.allChecks).check(target, extra)
  printProblems(target, problems)
}

def printProblems(target: List[Path], problems: List[List[Problem]]): Unit =
  println("--------------------------------------------------")
  if problems.flatten.isEmpty
  then println("NO PROBLEMS FOUND")
  else
    println("PROBLEMS:")
    for (t, ps) <- target.zip(problems) if !ps.isEmpty do
      println("--------------------------------------------------")
      println("Path: " + t)
      for p <- ps do
        println("----------------------")
        println(p)
  println("--------------------------------------------------")

// -------------------------------------------------------

class TASTyChecker(val checks: List[Check]):

  def check(target: Classpath, extra: Classpath): List[List[Problem]] =
    val context = Contexts.init(target ++ extra)
    given Context = context
    for
      entry <- target.entries.toList
    yield
      val checker = Checker(checks)
      val symbols = context.findSymbolsByClasspathEntry(entry)
      checker.check(symbols.map(_.tree.get))
      checker.problems

  def check(target: List[Path], extra: List[Path]): List[List[Problem]] =
    val targetClasspath = ClasspathLoaders.read(target)
    val extraClasspath = ClasspathLoaders.read(extra)
    check(targetClasspath, extraClasspath)
