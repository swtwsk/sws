package pl.edu.mimuw.sws
import scalaz.zio._
import scala.io.BufferedSource


case class ServerData(port: Int, log_file: String)

object ServerData {
  val default = ServerData(9999, "default.log.txt")
}

object ServerDataReader {
  def readConfigFile(configFile: String): IO[Exception, ServerData] =
    IO.bracket(FileIO.getAsBufferedSource(configFile))(FileIO.closeBufferedSource)(fromBufferedSource)

  def fromBufferedSource(bufferedSource: BufferedSource): IO[Exception, ServerData] =
    IO.syncException({
      var port = 9999
      var logf = "default.log.txt"
      for (line <- bufferedSource.getLines)
        line match {
          case line if line.startsWith("port: ") => port = line.substring(6).toInt
          case line if line.startsWith("log: ") => logf = line.substring(5)
          case default => ()
        }
      ServerData(port, logf)
    })
}
