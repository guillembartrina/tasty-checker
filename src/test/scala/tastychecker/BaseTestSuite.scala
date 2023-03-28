package tastychecker

import tastyquery.*
import tastyquery.Contexts.*

class BaseTestSuite extends munit.FunSuite:
  protected def testWithContext(testName: String)(body: Context ?=> Any) =
    test(testName) {
      body(using TestData.base_context)
    }

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