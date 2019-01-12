package pl.edu.mimuw.sws
import scalaz.zio._
import java.io.IOException
import java.net.Socket


case class Worker(serverData: Ref[ServerData], logQueue: Queue[Log]) {
  def talk(socket: Socket): IO[IOException, Unit] = for {
    optRequest <- WebIO.getRequest(socket)
    response = optRequest match {
      case Some(r) => HttpResponse(r.body)
      case None => HttpResponse("", statusCode = Http400)
    }
    _ <- WebIO.send(socket, response)
    _ <- WebIO.close(socket)
  } yield ()
}
