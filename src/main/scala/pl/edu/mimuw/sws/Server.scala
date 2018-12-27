package pl.edu.mimuw.sws

import java.io._
import java.net.ServerSocket

object Server {
  def main(args: Array[String]): Unit = {
    val server = new ServerSocket(9999)
    while (true) {
      val s = server.accept()

      val out = new PrintStream(s.getOutputStream)

      val response = Request(s).response
      out.print(response)
      out.flush()
      s.close()
    }
  }
}
