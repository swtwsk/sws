package pl.edu.mimuw.sws

import java.io._
import java.net.ServerSocket

object Server {
  def main(args: Array[String]): Unit = {
    val server = new ServerSocket(9999)
    while (true) {
      val s = server.accept()

      val out = new PrintStream(s.getOutputStream)

      val request = Request(s)

      val response = request match {
        case Some(r) => r.response
        case None => "HTTP/1.1 400 Bad Request\r\n\r\n"
      }
      out.print(response)
      out.flush()
      s.close()
    }
  }
}
