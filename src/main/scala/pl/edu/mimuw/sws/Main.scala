package pl.edu.mimuw.sws
import scalaz.zio.RTS


object Main {
  val rts: RTS = new RTS{}

  def main(args: Array[String]): Unit = {
    val configFile = if (args.length > 0) args(0) else "default.conf"
    val server = Server(configFile)
    rts.unsafeRun(server.run)
  }
}
