
lazy val root = project
    .in(file("."))
    .settings(
        scalaVersion := "3.3.0",
        name := "tasty-checker",
        description := " Reference checker for TASTy, the Scala 3 interchange format",
        version := "0.0.0",
        libraryDependencies += "ch.epfl.scala" %% "tasty-query" % "0.8.1"
    )