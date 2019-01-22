package pl.edu.mimuw.sws
import scalaz.zio._
import java.net.Socket

import scalaz.{-\/, \/, \/-}

case class Worker(serverData: Ref[ServerData], logQueue: Queue[Log], pathTree: PathNode) {
  def talk(socket: Socket): IO[Nothing, Unit] = {
    val base = for {
      request <- getRequest(socket)
      response <- getResponse(request)
      _ <- WebIO.send(socket, response)
    } yield keepAlive(request)
    lazy val cont: IO[Exception, Unit] = for {
      keep <- base
      next = if (keep) cont else IO.unit
      _ <- next
    } yield ()
    cont.ensuring(WebIO.close(socket))
        .catchAll(Log.to(logQueue))
  }

  def getRequest(socket: Socket): IO[Exception, Option[\/[HttpError, Request]]] =
    WebIO.getRequest(socket).map(Some(_)) // TODO socket timeout + catch to Option here

  def getResponse(optRequest: Option[\/[HttpError, Request]]): IO[Exception, Response] = IO.syncException(
    optRequest match {
      case Some(evr) => evr match {
        case \/-(r) => UrlResolver.resolve(r.path, pathTree) match {
          case \/-(v) => v(r)
          case -\/(error) => HttpErrorResponse(error, content = error.toString)
        }
        case -\/(error) => HttpErrorResponse(error, content = error.toString)
      }
      case None => HttpErrorResponse(Http408, Http408.toString)
    }
  )

  def keepAlive(optRequest: Option[\/[HttpError, Request]]): Boolean = {
    optRequest match {
      case Some(evr) => evr match {
        case \/-(r) => r.environ.get("Connection") match {
          case Some(spec) => spec == "keep-alive"
          case None => false // Not specified so no
        }
        case -\/(_) => false // HttpError so no
      }
      case None => false // No request so no
    }
  }
}
