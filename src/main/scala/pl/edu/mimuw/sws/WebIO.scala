package pl.edu.mimuw.sws
import scalaz.zio._
import java.io.PrintStream
import java.net.{ServerSocket, Socket}

import scalaz.\/

// abandon all hope ye who enter here for beyond lies only non-monadic IO
object WebIO {
  def listenOn(sd: ServerData): IO[Exception, ServerSocket] = IO.syncException(new ServerSocket(sd.port))
  def accept(socket: ServerSocket): IO[Exception, Socket] = IO.syncException(socket.accept())
  def getRequest(socket: Socket): IO[Exception, \/[HttpError, Request]] = IO.syncException(Request(socket))
  def send(socket: Socket, response: Response): IO[Exception, Unit] = IO.syncException({
      val out = new PrintStream(socket.getOutputStream)
      val responseArray = response.response.toArray
      out.write(responseArray, 0, responseArray.length)
      out.flush()
  })
  def close(socket: Socket): IO[Nothing, Unit] = IO.sync(socket.close())
}
