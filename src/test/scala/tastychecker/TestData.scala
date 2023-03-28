package tastychecker

import java.net.{URI}
import java.nio.file.{Path, Paths, FileSystems}

import tastyquery.*
import tastyquery.Classpaths.*
import tastyquery.jdk.ClasspathLoaders
import tastyquery.Contexts.*

object TestData:
  val extra_paths: List[Path] =
    FileSystems.getFileSystem(URI.create("jrt:/")).nn.getPath("modules", "java.base").nn
      :: sys.env.get("TASTYCHECKER_DEFAULTCLASSPATH").get.split(";").map(Paths.get(_)).toList

  val testlib_tastyquery_path: Path =
    Paths.get(sys.env.get("TASTYCHECKER_TESTLIB_TASTYQUERY_PATH").get)

  val extra_classpath = ClasspathLoaders.read(extra_paths)
  
  val testlib_tastyquery_classpath = ClasspathLoaders.read(List(testlib_tastyquery_path))

  val base_context = Contexts.init(extra_classpath)

  val testlib_tastyquery_context = Contexts.init(testlib_tastyquery_classpath ++ extra_classpath)
