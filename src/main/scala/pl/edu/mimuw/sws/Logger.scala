package pl.edu.mimuw.sws
import scalaz.zio._
import scalaz.zio.console._


case class Logger (sd: Ref[ServerData], lq: Queue[Log]) {
  val serverData: Ref[ServerData] = sd
  val logQueue: Queue[Log] = lq
  val log: IO[Nothing, Unit] = for {
    log <- lq.take
    sd <- serverData.get
    _ <- putStrLn("I'm logging to " + sd.log_file + "!")
    _ <- putStrLn("Log: " + log.name)
  } yield ()
}

