package tastychecker

import munit.FunSuite

import tastyquery.*
import tastyquery.Contexts.*

import tastyquery.Exceptions.*
import tastyquery.Spans.*
import tastyquery.Names.*
import tastyquery.Symbols.*
import tastyquery.Constants.*
import tastyquery.Trees.*
import tastyquery.Types.*
import java.{util => ju}

class TastyCheckerSuite extends BaseTestSuite:
  /*
  test("testlib_tastyquery_debug_toplevel_symbols") {
    val context = Contexts.init(TestData.testlib_tastyquery_classpath ++ TestData.extra_classpath)
    val symbols = context.findSymbolsByClasspathEntry(TestData.testlib_tastyquery_classpath.entries.toList(0))
    println("TOP-LEVEL SYMBOLS")
    println(symbols.toList)
  }
  */

  // -----

  /*
  test("testlib_tastyquery") {
    val problems = TASTyChecker(Check.allChecks).check(TestData.testlib_tastyquery_classpath, TestData.extra_classpath)
    assertEquals(problems(0), List.empty[Problem])
  }
  */

  /*
  test("testlib_tastyquery_alt1") {
    val context = Contexts.init(TestData.testlib_tastyquery_classpath ++ TestData.extra_classpath)
    given Context = context
    val entry = TestData.testlib_tastyquery_classpath.entries(0)
    val checker = Checker(Check.allChecks)

    for
      s <- context.findSymbolsByClasspathEntry(entry)
    do
      try
        checker.check(s.tree.get)
      catch case _: NotImplementedError | _: MemberNotFoundException | _: IllegalStateException => ()

    //assertEquals(checker.problems.size, 0)
    
    //TEMP    
    val pr = checker.problems(0)
    val x = pr.tree.asInstanceOf[Typed]
    println(x.tpt.asInstanceOf[TypeWrapper].tp.asInstanceOf[AppliedType].tycon
    .asInstanceOf[TypeRef].optSymbol.get.asDeclaringSymbol.tree)

    assertEquals(checker.problems, List.empty[Problem])
  }
  */

  test("testlib_tastyquery_alt2") {
    val context = Contexts.init(TestData.testlib_tastyquery_classpath ++ TestData.extra_classpath)
    given Context = context
    val entry = TestData.testlib_tastyquery_classpath.entries(0)

    for
      s <- context.findSymbolsByClasspathEntry(entry)
            .filter(x => !List("RefinedTypeTree", "MatchType", "ForExpressions", "RefinedType", "Function", "WithPartialFunction")
              .contains(x.name.toString()))
    do
      val checker = Checker(Check.checks(List("LSP")))
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
          case NotSubtype(a, b, tree)
            if a.toString.contains("symbol[Predef.Class]")
              || b.toString.contains("symbol[Predef.Class]")
            => false
          case NotSubtype(a, b, tree)
            if a.widen.toString.contains("WildcardTypeBounds(TypeBounds(TypeRef(PackageRef(scala),")
              || b.widen.toString.contains("WildcardTypeBounds(TypeBounds(TypeRef(PackageRef(scala),")
            => false
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

    //assertEquals(checker.problems, List.empty[Problem])
  }

  /*
  test("testlib_tastyquery_specific") {
    val context = Contexts.init(TestData.testlib_tastyquery_classpath ++ TestData.extra_classpath)
    val symbols = context.findSymbolsByClasspathEntry(TestData.testlib_tastyquery_classpath.entries.toList(0))
    
    val checker = Checker(Check.allChecks)
    for
      s <- symbols
    do
      try
        checker.check(s.tree(using context).get)(using context)
      catch case e: MemberNotFoundException =>
        println("Trace")
        e.printStackTrace()
        println("Error: " + s"[$e]" + s.name.toDebugString)
        println("# Tree " + s.tree(using context))
        println("-----------------------------")
  }
  */

  /*
  test("testlib_tastyquery_single") {
    val sym_name = "simple_trees.ScalaEnum"
    
    val context = Contexts.init(TestData.testlib_tastyquery_classpath ++ TestData.extra_classpath)
    val symbol = context.findStaticModuleClass(sym_name)
    val tree = symbol.tree(using context).get

    println("Symbol: " + symbol)
    println("TREE")
    println(tree)

    val checker = Checker(Check.allChecks)
    checker.check(tree)(using context)
    for
      p <- checker.problems
    do
      println("--------------")
      println(p)

    assert(checker.problems.isEmpty)
  }
  */

  /*
  test("temp0") {
    import java.net.{URI}
    import java.nio.file.{Path, Paths, FileSystems}
    import tastyquery.jdk.ClasspathLoaders

    val path = Paths.get("../hello")
    val classpath = ClasspathLoaders.read(List(path))
    val context = Contexts.init(classpath ++ TestData.extra_classpath)
    given Context = context
    val symbols = context.findSymbolsByClasspathEntry(classpath.entries.toList(0))
    println("TOP-LEVEL SYMBOLS")
    println(symbols.toList.foreach(x => println(x.tree(using context))))

  }
  */

  testSymbolWithContext(TestData.testlib_tastyquery_context)("testhello")("hellotest.Hello[$]") { symbol =>

    println(symbol.asDeclaringSymbol.declarations(3).tree.get.asInstanceOf[ValDef].rhs
    .get.asInstanceOf[Match].cases(2).pattern)

    val checker = Checker(Check.checks(List("PseudoLSPMatching")))
    checker.check(symbol.tree)
    
    checker.problems.foreach(x => { println(x); println() })

    //Looks like the type of meth is not correct? Or maybe isSubtype doesn't work for SAMs
  }
