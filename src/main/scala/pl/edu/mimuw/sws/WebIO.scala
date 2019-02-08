package pl.edu.mimuw.sws
import scalaz.zio._
import java.io.{InputStream, PrintStream}
import java.net.{ServerSocket, Socket}

import scalaz.\/

// abandon all hope ye who enter here for beyond lies only non-monadic IO
object WebIO {
  // server side
  def listenOn(sd: ServerData): IO[Exception, ServerSocket] = IO.syncException(new ServerSocket(sd.port))
  def accept(socket: ServerSocket): IO[Exception, Socket] = IO.blocking(socket.accept())
  def getRequest(socket: Socket): IO[Exception, \/[HttpError, Request]] = IO.syncException(Request(socket))
  def sendResponse(socket: Socket, response: Response): IO[Exception, Unit] = IO.syncException({
      val out = new PrintStream(socket.getOutputStream, true)
      val responseArray = response.response.toArray
      out.write(responseArray, 0, responseArray.length)
  })

  // client side
  def connectTo(hostname: String, port: Int): IO[Exception, Socket] =
    IO.syncException(new Socket(hostname, port))
  def sendBytes(socket: Socket, array: Array[Byte]): IO[Exception, Unit] =
    IO.syncException({
      val out = new PrintStream(socket.getOutputStream, true)
      out.write(array, 0, array.length)
    })
  def sendT[T](socket: Socket, t: T): IO[Exception, Unit] =
    IO.syncException({
      val out = new PrintStream(socket.getOutputStream, true)
      out.print(t)
    })
  def getBytes(socket: Socket, size: Int): IO[Exception, (Int, Array[Byte])] =
    IO.syncException({
      val buffer = new Array[Byte](size)
      val instrm = socket.getInputStream
      (instrm.read(buffer), buffer)
    })

  def close(socket: Socket): IO[Nothing, Unit] = IO.sync(socket.close())
}
