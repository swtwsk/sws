package pl.edu.mimuw.sws
import scalaz.zio._
import java.net.Socket
import java.util.concurrent.TimeUnit

import scalaz.zio.duration.Duration
import scalaz.{-\/, \/, \/-}
import pl.edu.mimuw.sws.UrlResolver.PathNode

case class Worker(serverData: Ref[ServerData], logQueue: Queue[Log], pathTree: PathNode) {
  def talk(socket: Socket): IO[Nothing, Unit] =
    getRequest(socket).flatMap(getResponse).flatMap(WebIO.send(socket, _))
                      .ensuring(WebIO.close(socket))
                      .catchAll(Log.to(logQueue))

  def getRequest(socket: Socket): IO[Exception, Option[\/[HttpError, Request]]] =
    WebIO.getRequest(socket).timeout(Duration(5, TimeUnit.SECONDS))

  def getResponse(optRequest: Option[\/[HttpError, Request]]): IO[Exception, Response] = IO.syncException(
    optRequest match {
      case Some(evr) => evr match {
        case \/-(r) => UrlResolver.resolve(r.path, pathTree) match {
          case \/-(v) => v(r)
          case -\/(error) => HttpResponse("NOT FOUND", statusCode = error)
        }
        case -\/(error) => HttpResponse("", statusCode = error)
      }
      case None => HttpResponse("TIMEOUT", statusCode = Http400)
    }
  )
}
