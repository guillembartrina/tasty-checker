
ThisBuild / scalaVersion := "3.2.2"

lazy val root = project
  .in(file("."))
  .dependsOn(testlib_tastyquery % Test)
  .settings(
    name := "tasty-checker",
    description := " Reference checker for TASTy, the Scala 3 interchange format",
    version := "0.0.0",
    libraryDependencies ++= Seq(
      "ch.epfl.scala" %% "tasty-query" % "0.6.0",
      "org.scalameta" %% "munit" % "0.7.29" % Test
    ),
    envVars += {
      "TASTYCHECKER_DEFAULTCLASSPATH" -> Attributed.data((Compile / fullClasspath).value).map(_.getAbsolutePath).mkString(";")
    },
    Test / envVars += {     
      "TASTYCHECKER_TESTLIB_TASTYQUERY_PATH" -> (testlib_tastyquery / Compile / classDirectory).value.getAbsolutePath
    },
    fork := true
  )

lazy val testlib_tastyquery = project
  .in(file("src/test/lib/tastyquery"))
  .settings(
    publish / skip := true
  )
