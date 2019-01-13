package pl.edu.mimuw.sws
import scalaz.zio._
import java.io.IOException
import java.net.Socket

import scalaz.{-\/, \/-}


case class Worker(serverData: Ref[ServerData], logQueue: Queue[Log]) {
  def talk(socket: Socket): IO[IOException, Unit] = for {
    optRequest <- WebIO.getRequest(socket)
    response = optRequest match {
      case \/-(r) => HttpResponse(r.responseBody)
      case -\/(error) => HttpResponse("", statusCode = error)
    }
    _ <- WebIO.send(socket, response)
    _ <- WebIO.close(socket)
  } yield ()
}
