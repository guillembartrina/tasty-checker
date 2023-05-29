package tastychecker

import tastyquery.*
import tastyquery.Contexts.*
import tastyquery.Names.*
import tastyquery.Symbols.*


class BaseTestSuite extends munit.FunSuite:
  protected def testWithContext(context: Context)(testName: String)(body: Context ?=> Any) =
    test(testName) {
      body(using context)
    }

  protected val testWithBaseContext = testWithContext(TestData.base_context)

  protected def testSymbolWithContext(context: Context)(testName: String)(symbolPath: String)(body: Symbol => Context ?=> Any) =
    val path = symbolPath.split("\\.").map(s =>
        if s.endsWith("[$]") then moduleClassName(s.stripSuffix("[$]"))
        else if s.endsWith("/T") then typeName(s.stripSuffix("/T"))
        else termName(s)
      ).toList
    testWithContext(context)(testName) {
      body(context.findSymbolFromRoot(path))
    }

  protected val testSymbolWithBaseContext = testSymbolWithContext(TestData.base_context)


  protected def assertProblems(using munit.Location)(actual: List[Problem], expected: List[Problem]): Unit =
    val as = actual.toSet.map(_.toString)
    val es = expected.toSet.map(_.toString)
    val notexpected = as &~ es
    val notactual = es &~ as
    if notexpected.nonEmpty || notactual.nonEmpty then
      var msg = "Assertion on problem list failed."
      if notexpected.nonEmpty then msg += notexpected.mkString("\nUnexpected problems:\n* ", "\n* ", "")
      if notactual.nonEmpty then msg += notactual.mkString("\nMissing problems:\n* ", "\n* ", "")
      fail(msg)

  protected def assertNoProblems(using munit.Location)(actual: List[Problem]): Unit =
    assertProblems(actual, List.empty[Problem])


  import tastyquery.Trees.Tree
  extension (tree: Tree)
    def print: Unit = println(munitPrint(clue(tree)))

  import tastyquery.Types.Type
  extension (tpe: Type)
    def print: Unit = println(munitPrint(clue(tpe)))
