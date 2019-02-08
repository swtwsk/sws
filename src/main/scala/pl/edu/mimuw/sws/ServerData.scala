package pl.edu.mimuw.sws
import scalaz.zio._
import scala.io.BufferedSource
import Severity._


case class ServerData(var port: Int,
                      var logFile: String,
                      var severityTerm: Severity,
                      var severityFile: Severity)

object ServerData {
  val default = ServerData(9999,
                           "default.log.txt",
                            Severity.Debug,
                            Severity.Informational)
}

object ServerDataReader {
  def readConfigFile(configFile: String): IO[Exception, ServerData] =
    IO.bracket(FileIO.getBufferedSource(configFile))(FileIO.closeBufferedSource)(fromBufferedSource)

  def fromBufferedSource(bufferedSource: BufferedSource): IO[Exception, ServerData] =
    IO.syncException({
      var serverData = ServerData.default
      for (line <- bufferedSource.getLines)
        line match {
          case line if line.startsWith("port: ") => serverData.port = line.substring(6).toInt
          case line if line.startsWith("log: ") => serverData.logFile = line.substring(5)
          case _ => ()
        }
      serverData
    })
}
