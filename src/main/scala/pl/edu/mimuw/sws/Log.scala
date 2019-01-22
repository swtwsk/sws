package pl.edu.mimuw.sws
import scalaz.zio._


object Severity extends Enumeration {
  type Severity = Value
  val Emergency     = Value(0) // system is unusable
  val Alert         = Value(1) // action must be taken immediately
  val Critical      = Value(2) // critical conditions
  val Error         = Value(3) // error conditions
  val Warning       = Value(4) // warning conditions
  val Notice        = Value(5) // normal but significant condition
  val Informational = Value(6) // informational messages
  val Debug         = Value(7) // debug-level messages
}
import Severity._


case class Log(severity: Severity, message: String)

object Log {
  def to(logQueue: Queue[Log])(log: Log): IO[Nothing, Unit] =
    logQueue.offer(log) *> IO.unit

  def exception(logQueue: Queue[Log])(e: Exception): IO[Nothing, Unit] =
    error(logQueue)("Error: " + e.getMessage)

  def emergency(logQueue: Queue[Log])(m: String): IO[Nothing, Unit] =
    to(logQueue)(Log(Severity.Emergency, m))

  def alert(logQueue: Queue[Log])(m: String): IO[Nothing, Unit] =
    to(logQueue)(Log(Severity.Alert, m))

  def critical(logQueue: Queue[Log])(m: String): IO[Nothing, Unit] =
    to(logQueue)(Log(Severity.Critical, m))

  def error(logQueue: Queue[Log])(m: String): IO[Nothing, Unit] =
    to(logQueue)(Log(Severity.Error, m))

  def warning(logQueue: Queue[Log])(m: String): IO[Nothing, Unit] =
    to(logQueue)(Log(Severity.Warning, m))

  def notice(logQueue: Queue[Log])(m: String): IO[Nothing, Unit] =
    to(logQueue)(Log(Severity.Notice, m))

  def informational(logQueue: Queue[Log])(m: String): IO[Nothing, Unit] =
    to(logQueue)(Log(Severity.Informational, m))

  def debug(logQueue: Queue[Log])(m: String): IO[Nothing, Unit] =
    to(logQueue)(Log(Severity.Debug, m))

}
