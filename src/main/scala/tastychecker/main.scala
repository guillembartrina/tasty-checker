package tastychecker

import java.nio.file.{Path, Paths, FileSystems}


@main def main() = {
  val default: List[Path] = List()
  val target: List[Path] = List()
  val extra: List[Path] = List() ++ default

  val checker = TASTyChecker(TreeCheck.allChecks, TreeFilter.default)
  val results = checker.check(target, extra)
  TASTyChecker.printResults(results)
}
