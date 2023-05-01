package tastychecker

import tastyquery.*
import tastyquery.Contexts.*

import tastyquery.Exceptions.*
import tastyquery.Spans.*
import tastyquery.Names.*
import tastyquery.Symbols.*
import tastyquery.Constants.*
import tastyquery.Trees.*
import tastyquery.Types.*

class TestlibsTestSuite extends BaseTestSuite:

  test("testlib_dummy") {
    val problems = TASTyChecker(Check.allChecks).check(TestData.testlib_dummy_classpath, TestData.extra_classpath)
    TASTyChecker.printProblems(List(TestData.testlib_dummy_path), problems)
  }

  test("testlib_tastyquery") {
    val problems = TASTyChecker(Check.allChecks).check(TestData.testlib_tastyquery_classpath, TestData.extra_classpath)
    TASTyChecker.printProblems(List(TestData.testlib_tastyquery_path), problems)
  }

  testWithContext(TestData.testlib_dummy_context)("testlib_dummy_closer") {
    val symbolFilter = List()

    val entry = TestData.testlib_dummy_classpath.entries(0)
    for
      s <- summon[Context].findSymbolsByClasspathEntry(entry)
        .filter(x => !symbolFilter.contains(x.name.toString()))
    do
      val checker = Checker(Check.allChecks)
      try
        checker.check(s.tree.get)
      catch {
        case e: (NotImplementedError | IllegalStateException) => println("ERROR[I]: " + s)
        case e: MemberNotFoundException => println("ERROR[MNF]: " + s)
        case e: NoSuchElementException => println("ERROR[NSE]: " + s)
      }
      val problems = checker.problems
      if !problems.isEmpty
      then
        println("----------------------------------------")
        println("PROBLEMS FOUND IN " + s.name)
        for p <- problems do println(p)
  }

  testWithContext(TestData.testlib_tastyquery_context)("testlib_tastyquery_closer") {
    val symbolFilter = List("DependentMethod", "TopLevelOpaqueTypeAlias$package", "AnyMethods", "MyArrayOps", "TypesFromTASTy")

    val entry = TestData.testlib_tastyquery_classpath.entries(0)
    for
      s <- summon[Context].findSymbolsByClasspathEntry(entry)
        .filter(x => !symbolFilter.contains(x.name.toString()))
    do
      val checker = Checker(Check.allChecks)
      try
        checker.check(s.tree.get)
      catch {
        case e: (NotImplementedError | IllegalStateException) => println("ERROR[I]: " + s)
        case e: MemberNotFoundException => println("ERROR[MNF]: " + s)
        case e: NoSuchElementException => println("ERROR[NSE]: " + s)
      }
      val problems = checker.problems
      if !problems.isEmpty
        then
          println("----------------------------------------")
          println("PROBLEMS FOUND IN " + s.name)
          for p <- problems do println(p)
  }

  /*
  test("testlib_tastyquery_alt2") {
    val context = Contexts.init(TestData.testlib_tastyquery_classpath ++ TestData.extra_classpath)
    given Context = context
    val entry = TestData.testlib_tastyquery_classpath.entries(0)

    for
      s <- context.findSymbolsByClasspathEntry(entry)
            .filter(x => !List("DependentMethod" /*TermRef issue*/).contains(x.name.toString()))
    do
      val checker = Checker(Check.allChecks) // "TypeParamBounds"
      try
        checker.check(s.tree.get)
      catch {
        case e: (NotImplementedError | MemberNotFoundException | IllegalStateException) => println("O - ERROR " + s)
        case e: NoSuchElementException => println("NSE - ERROR " + s)
      }
      finally
        ()

      //filtering
      val problems = (
        checker.problems
        .filter(p => p match
          case NotConformsType(a, b, tree)
            if a.toString.contains("symbol[Predef.Class]")
              || b.toString.contains("symbol[Predef.Class]")
            => false // Class
          case NotConformsType(a, b, tree)
            if a.toString.contains("symbol[class lang.Class]")
              || b.toString.contains("symbol[class lang.Class]")
            => false // Class
          case NotConformsType(a, b, tree)
            if a.toString.contains("TermRef(NoPrefix, symbol[apply>x])")
              || b.toString.contains("TermRef(NoPrefix, symbol[apply>x])")
            => false // Opaque
          case NotConformsType(a, b, tree)
            if a.toString.contains("AppliedType(TypeRef(ThisType(TypeRef")
              || b.toString.contains("AppliedType(TypeRef(ThisType(TypeRef")
            => false // Opaque
          case _ => true
        )
      )
      
      if !problems.isEmpty
      then
        println("----------------------------------------")
        println("PROBLEMS FOUND IN " + s.name)

        for
          p <- problems
        do
          println(p)
  }
  */


