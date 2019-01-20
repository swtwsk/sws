package pl.edu.mimuw.sws
import scalaz.zio._
import java.io.IOException
import java.net.Socket

import scalaz.{-\/, \/-}
import pl.edu.mimuw.sws.UrlResolver.PathNode

case class Worker(serverData: Ref[ServerData], logQueue: Queue[Log], pathTree: PathNode) {
  def talk(socket: Socket): IO[IOException, Unit] = for {
    optRequest <- WebIO.getRequest(socket)
    response = optRequest match {
      case \/-(r) => UrlResolver.resolve(r.path, pathTree) match {
        case \/-(v) => v(r)
        case -\/(error) => HttpResponse("NOT FOUND", statusCode = error)
      }
      case -\/(error) => HttpResponse("", statusCode = error)
    }
    _ <- WebIO.send(socket, response)
    _ <- WebIO.close(socket)
  } yield ()
}
