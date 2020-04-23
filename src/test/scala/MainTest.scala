import java.nio.file.{Path, Paths, Files}

import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.JavaConverters._

class MainTest extends AnyFlatSpec with Matchers {
  trait services {
    implicit val fs: RealFileSystem[Id] = new RealFileSystem[Id]
    implicit val printer: ConsolePathPrinter[Id] = new ConsolePathPrinter[Id]
    val program = new Program[Id, Path, Path]
    val dir: Path = Paths.get("/tmp")
    program.run(dir)
  }

  "Program.run" should "work correctly" in new services {
    Files.exists(dir.resolve("test_dir")) shouldBe true
    val got: List[Path] =
      Files.walk(dir.resolve("test_dir")).sorted().iterator().asScala.toList
    val expected: List[Path] = List(
      "/tmp/test_dir",
      "/tmp/test_dir/b",
      "/tmp/test_dir/b/bar",
      "/tmp/test_dir/b/baz",
      "/tmp/test_dir/f",
      "/tmp/test_dir/f/foo"
    ).map(f => Paths.get(f))
    got shouldBe expected
  }
}