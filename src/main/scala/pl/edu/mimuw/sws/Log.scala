package pl.edu.mimuw.sws
import scalaz.zio._
import scalaz.zio.console._

case class Log(name: String)

object Log {
  def to(logQueue: Queue[Log])(e: Exception): IO[Nothing, Unit] =
    logQueue.offer(Log(e.getMessage)) *> putStrLn(e.getMessage)
}
