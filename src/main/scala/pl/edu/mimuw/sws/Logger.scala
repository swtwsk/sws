package pl.edu.mimuw.sws
import scalaz.zio._
import scalaz.zio.console._
import Severity._


case class Logger (serverDataRef: Ref[ServerData],
                   logQueue: Queue[Log],
                   severity: Severity) {
  val log: IO[Nothing, Unit] = for {
    log <- logQueue.take
    serverData <- serverDataRef.get
    print = if (log.severity <= severity)
              putStrLn("Log (" + log.severity + "): " + log.message)
            else IO.unit
    _ <- print
  } yield ()
}
