package pl.edu.mimuw.sws
import pl.edu.mimuw.sws.UrlResolver.View
import scalaz.zio.RTS


abstract class ServerMain {
  val rts: RTS = new RTS{}

  val urls: List[(String, View)]

  def main(args: Array[String]): Unit = {
    val configFile = if (args.length > 0) args(0) else "default.conf"
    val server = Server(configFile, urls)
    rts.unsafeRun(server.run)
  }
}
