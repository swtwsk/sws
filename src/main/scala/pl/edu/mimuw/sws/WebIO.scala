package pl.edu.mimuw.sws
import scalaz.zio._
import java.io.PrintStream
import java.net.{ServerSocket, Socket}

import scalaz.\/

// abandon all hope ye who enter here for beyond lies only non-monadic IO
object WebIO {
  def listenOn(sd: ServerData): IO[Nothing, ServerSocket] = IO.point(new ServerSocket(sd.port))
  def accept(socket: ServerSocket): IO[Nothing, Socket] = IO.point(socket.accept())
  def getRequest(socket: Socket): IO[Nothing, \/[HttpError, Request]] = IO.point(Request(socket))
  def send(socket: Socket, response: Response): IO[Nothing, Unit] = IO.point({
      val out = new PrintStream(socket.getOutputStream)
      out.print(response)
      out.flush()
  })
  def close(socket: Socket): IO[Nothing, Unit] = IO.point(socket.close())
}
