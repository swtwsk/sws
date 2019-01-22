package pl.edu.mimuw.sws
import pl.edu.mimuw.sws.UrlResolver.{Controller, IOController}
import scalaz.zio.RTS


abstract class ServerMain {
  private final val rts: RTS = new RTS{}

  val urls: List[(String, Controller)]
  val urlsIO: List[(String, IOController)] = List()

  val static: Option[(String, String)] = None

  final def main(args: Array[String]): Unit = {
    val configFile = if (args.length > 0) args(0) else "default.conf"
    val server = Server(configFile, urls, urlsIO, static)
    rts.unsafeRunSync(server.run)
  }
}
