package pl.edu.mimuw.sws
import scalaz.zio._


case class ServerData(port: Int, log_file: String)

object ServerDataReader {
  def readConfigFile(configFile: String): IO[Nothing, ServerData] = {
    IO.point(ServerData(9999, "default.log.txt"))
  }
}
