import cats.syntax.all._
import cats.effect._
import cats.effect.concurrent.MVar
import scala.concurrent.duration._

object Main extends IOApp {
  def runPrinter(mvar: MVar[IO, String]): Resource[IO, Unit] = {
    def run: IO[Unit] = for {
      x <- mvar.take
      _ <- IO(println(x))
      _ <- run
    } yield ()

    Resource.make(run.start)(_.cancel).void
  }

  def runCounter(mvar: MVar[IO, String]): Resource[IO, Unit] = {
    def run(x: Int): IO[Unit] = for {
      _ <- mvar.put(x.toString)
      _ <- IO.sleep(1.seconds)
      _ <- run(x + 1)
    } yield ()

    Resource.make(run(1).start)(_.cancel).void
  }

  val program: Resource[IO, Unit] = for {
    mvar <- Resource.make(MVar.empty[IO, String])(_ => IO.unit)
    _ <- runPrinter(mvar)
    _ <- runCounter(mvar)
  } yield ()

  override def run(args: List[String]): IO[ExitCode] = program.use(_ => IO.never)
}