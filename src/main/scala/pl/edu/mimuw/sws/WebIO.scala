package pl.edu.mimuw.sws
import scalaz.zio._
import java.io.PrintStream
import java.net.{ServerSocket, Socket}

// abandon all hope ye who enter here for beyond lies only non-monadic IO
object WebIO {
  def listenOn(sd: ServerData): IO[Exception, ServerSocket] = IO.syncException(new ServerSocket(sd.port))
  def accept(socket: ServerSocket): IO[Exception, Socket] = IO.syncException(socket.accept())
  def getRequest(socket: Socket): IO[Exception, Option[Request]] = IO.syncException(Request(socket))
  def send(socket: Socket, response: Response): IO[Exception, Unit] = IO.syncException({
      val out = new PrintStream(socket.getOutputStream)
      out.print(response)
      out.flush()
  })
  def close(socket: Socket): IO[Nothing, Unit] = IO.sync(socket.close())
}
