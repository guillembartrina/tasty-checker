
lazy val root = project
    .in(file("."))
    .settings(
        scalaVersion := "3.2.2",
        name := "tasty-checker",
        description := " Reference checker for TASTy, the Scala 3 interchange format",
        version := "0.0.0",
        libraryDependencies += "ch.epfl.scala" %% "tasty-query" % "0.7.3"
    )