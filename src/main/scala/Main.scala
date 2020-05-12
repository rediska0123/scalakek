import java.nio.file.{FileAlreadyExistsException, Files, Path, Paths, StandardCopyOption}

import cats.{Applicative, Id, Monad}
import cats.syntax.all._
import cats.instances.list._

import scala.collection.JavaConverters._
import scala.language.higherKinds

trait MkDir[F[_], Dir] {
  def mkDir(dir: Dir, name: String): F[Dir]
}

trait MkFile[F[_], Dir, File] {
  def mkFile(dir: Dir, name: String): F[File]
}

trait Printer[F[_], File] {
  def printName(file: File): F[Unit]
}

trait GetFiles[F[_], Dir, File] {
  def getFiles(dir: Dir): F[List[File]]
}

trait GetFirstLetter[F[_], File] {
  def getFirstLetter(file: File): F[Char]
}

trait MoveFile[F[_], Dir, File] {
  def moveFile(file: File, dir: Dir): F[Unit]
}

class Program[F[_], Dir, File](implicit
                               F: Monad[F],
                               mkDir: MkDir[F, Dir],
                               mkFile: MkFile[F, Dir, File],
                               printer: Printer[F, File],
                               getFiles: GetFiles[F, Dir, File],
                               getFirstLetter: GetFirstLetter[F, File],
                               moveFile: MoveFile[F, Dir, File]) {
  def run(dir: Dir): F[Unit] = for {
    testDir <- mkDir.mkDir(dir, "test_dir")
    _ <- mkFile.mkFile(testDir, "foo")
    _ <- mkFile.mkFile(testDir, "bar")
    _ <- mkFile.mkFile(testDir, "baz")


    files <- getFiles.getFiles(testDir)
    _ <- files.traverse(f => {
      for {
        _ <- printer.printName(f)
        ch <- getFirstLetter.getFirstLetter(f)
        d <- mkDir.mkDir(testDir, ch.toString)
        _ <- moveFile.moveFile(f, d)
      } yield ()
    })
  } yield ()
}

class RealFileSystem[F[_] : Applicative] extends MkDir[F, Path]
  with MkFile[F, Path, Path]
  with GetFiles[F, Path, Path]
  with GetFirstLetter[F, Path]
  with MoveFile[F, Path, Path] {

  override def mkDir(dir: Path, name: String): F[Path] =
    Files.createDirectories(dir.resolve(name)).pure[F]

  override def mkFile(dir: Path, name: String): F[Path] =
    Files.createFile(dir.resolve(name)).pure[F]

  override def getFiles(dir: Path): F[List[Path]] =
    Files.list(dir).filter(f => Files.isRegularFile(f)).iterator().asScala.toList.pure[F]

  override def getFirstLetter(file: Path): F[Char] =
    file.getFileName.toString.charAt(0).pure[F]

  override def moveFile(file: Path, dir: Path): F[Unit] =
    Files.move(file, dir.resolve(file.getFileName), StandardCopyOption.REPLACE_EXISTING).pure[F].void
}

class ConsolePathPrinter[F[_] : Applicative] extends Printer[F, Path] {
  override def printName(file: Path): F[Unit] = println(file.getFileName).pure[F]
}

object TypeClasses {
  def main(args: Array[String]): Unit = {
    implicit val fs: RealFileSystem[Id] = new RealFileSystem[Id]
    implicit val printer: ConsolePathPrinter[Id] = new ConsolePathPrinter[Id]
    val program = new Program[Id, Path, Path]

    program.run(Paths.get("."))
  }
}