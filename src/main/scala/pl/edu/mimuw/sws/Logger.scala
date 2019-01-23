package pl.edu.mimuw.sws
import java.io.PrintWriter

import scalaz.zio._
import scalaz.zio.console._


class Logger (serverDataRef: Ref[ServerData],
                   logQueue: Queue[Log]) {

  var fn = Option.empty[String]
  var pw = Option.empty[PrintWriter]
  def setFn(n: String): IO[Nothing, Unit] = IO.sync({fn = Some(n)})
  def setPw(p: PrintWriter): IO[Nothing, Unit] = IO.sync({pw = Some(p)})

  val init: IO[Exception, Unit] =
  for {
    serverData <- serverDataRef.get
    _ <- setFn(serverData.logFile)
    _ <- fn match {
      case Some(n) => FileIO.getPrintWriter(n).flatMap(setPw)
      case None => IO.unit
    }
    _ <- putStrLn("Logger: init done")
  } yield ()

  val fnsh: IO[Nothing, Unit] =
  pw match {
    case Some(w) => FileIO.closePrintWriter(w)
    case None => IO.unit
  }

  val check: IO[Exception, Unit] =
  for {
    serverData <- serverDataRef.get
    _ <- if (fn.contains(serverData.logFile))
           IO.unit
         else
           fnsh *> init
  } yield ()

  val log: IO[Exception, Unit] =
  for {
    serverData <- serverDataRef.get
    log <- logQueue.take
    logs = "Log (" + log.severity + "): " + log.message
    printWriter = pw.get
    _ <- if (log.severity <= serverData.severityTerm)
           putStrLn(logs)
         else
           IO.unit
    _ <- if (log.severity <= serverData.severityFile)
           FileIO.append(printWriter)(logs)
         else
           IO.unit
  } yield ()

  val run: IO[Nothing, Unit] = IO.bracket(init)(_ => fnsh)(_ => (check *> log).forever)
    .catchAll(e => Log.emergency(logQueue)(e.getMessage))

}
