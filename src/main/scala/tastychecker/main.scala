package tastychecker

import java.nio.file.{Path, Paths, FileSystems}


@main def main() = {
  val default: List[Path] = List()

  val target: List[Path] = List()
  val extra: List[Path] = List() ++ default


  val results = TASTyChecker(Check.allChecks).check(target, extra)
  TASTyChecker.printResults(results)
}
