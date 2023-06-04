package tastychecker


class TestlibsTestSuite extends BaseTestSuite:

  test("testlib_tastyquery") {
    val results = TASTyChecker(TreeCheck.allChecks, TreeFilter.default).check(TestData.testlib_tastyquery_path :: Nil, TestData.extra_paths)
    TASTyChecker.printResults(results)
  }

  test("testlib_dummy") {
    val results = TASTyChecker(TreeCheck.allChecks, TreeFilter.default).check(TestData.testlib_dummy_path :: Nil, TestData.extra_paths)
    TASTyChecker.printResults(results)
  }
