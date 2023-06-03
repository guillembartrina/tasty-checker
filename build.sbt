
ThisBuild / scalaVersion := "3.3.0"

lazy val root = project
  .in(file("."))
  .dependsOn(test_auxiliar % Test)
  .dependsOn(testlib_tastyquery % Test)
  .dependsOn(testlib_dummy % Test)
  .settings(
    name := "tasty-checker",
    description := " Reference checker for TASTy, the Scala 3 interchange format",
    version := "0.0.0",
    libraryDependencies ++= Seq(
      "ch.epfl.scala" %% "tasty-query" % "0.8.1",
      "org.scalameta" %% "munit" % "0.7.29" % Test
    ),
    envVars += {
      "TASTYCHECKER_DEFAULTCLASSPATH" -> Attributed.data((Compile / fullClasspath).value).map(_.getAbsolutePath).mkString(";")
    },
    Test / envVars += {     
      "TASTYCHECKER_TEST_AUXILIAR_PATH" -> (test_auxiliar / Compile / classDirectory).value.getAbsolutePath
    },
    Test / envVars += {     
      "TASTYCHECKER_TESTLIB_TASTYQUERY_PATH" -> (testlib_tastyquery / Compile / classDirectory).value.getAbsolutePath
    },
    Test / envVars += {     
      "TASTYCHECKER_TESTLIB_DUMMY_PATH" -> (testlib_dummy / Compile / classDirectory).value.getAbsolutePath
    },
    fork := true
  )

lazy val test_auxiliar = project
  .in(file("src/test/scala/tastychecker/auxiliar"))
  .settings(
    publish / skip := true
  )

lazy val testlib_tastyquery = project
  .in(file("src/test/lib/tastyquery"))
  .settings(
    publish / skip := true
  )

lazy val testlib_dummy = project
  .in(file("src/test/lib/dummy"))
  .settings(
    publish / skip := true
  )
