package pl.edu.mimuw.sws
import pl.edu.mimuw.sws.UrlResolver.{Controller, IOController}
import scalaz.zio.RTS


abstract class ServerMain {
  private final val rts: RTS = new RTS{}

  val defaultConfigFile: String = "default.conf"

  val urls: List[(String, Controller)]
  val urlsIO: List[(String, IOController)] = List()

  val static: Option[(String, String)] = None
  val favicon: Option[String] = None

  final def main(args: Array[String]): Unit = {
    val configFile = if (args.length > 0) args(0) else defaultConfigFile
    val server = Server(configFile, urls, urlsIO, static, favicon)
    rts.unsafeRunSync(server.run)
  }
}
