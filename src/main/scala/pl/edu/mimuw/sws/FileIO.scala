package pl.edu.mimuw.sws
import scalaz.zio._
import scala.io.BufferedSource


object FileIO {
  def getAsBufferedSource(configFile: String): IO[Exception, BufferedSource] =
    IO.syncException(scala.io.Source.fromFile(configFile))

  def closeBufferedSource(bufferedSource: BufferedSource): IO[Nothing, Unit] =
    IO.sync(bufferedSource.close())
}
