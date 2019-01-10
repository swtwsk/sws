package pl.edu.mimuw.sws
import scalaz.zio._
import java.io.IOException
import java.net.ServerSocket


case class Worker(serverData: Ref[ServerData], logQueue: Queue[Log], serverSocket: ServerSocket) {
  val talk: IO[IOException, Unit] = for {
    socket <- WebIO.accept(serverSocket)
    optRequest <- WebIO.getRequest(socket)
    response = optRequest match {
      case Some(r) => HttpResponse(r.body)
      case None => HttpResponse("", statusCode = Http400)
    }
    _ <- WebIO.send(socket, response)
    _ <- WebIO.close(socket)
  } yield ()
}
