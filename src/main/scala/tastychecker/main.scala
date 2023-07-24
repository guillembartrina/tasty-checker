package tastychecker

import java.nio.file.{Path, Paths, FileSystems}


@main def main(targetPaths: String, extraPaths: String) = {
  import java.net.URI
  val default: List[Path] =
    FileSystems.getFileSystem(URI.create("jrt:/")).nn.getPath("modules", "java.base").nn
      :: sys.env.get("TASTYCHECKER_DEFAULTCLASSPATH").get.split(";").map(Paths.get(_)).toList

  val target: List[Path] = targetPaths.split(";").map(Paths.get(_)).toList

  val (defaultSign, extraPathsList) = extraPaths.split(";").partition(_ == "#")
  val extra: List[Path] = extraPathsList.map(Paths.get(_)).toList ++ (if defaultSign.isEmpty then Nil else default)

  val checker = TASTyChecker(TreeCheck.allChecks, TreeFilter.default)
  val results = checker.check(target, extra)
  TASTyChecker.printResults(results)
}
