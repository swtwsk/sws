package pl.edu.mimuw.sws
import scalaz.zio._
import java.net.Socket

import scalaz.{-\/, \/, \/-}

case class Worker(serverData: Ref[ServerData], logQueue: Queue[Log], pathTree: PathNode, resolver: UrlResolver) {
  def talk(socket: Socket): IO[Nothing, Unit] = {
    val base = for {
      request <- getRequest(socket)
      response <- getResponse(request)
      _ <- send(socket, response)
    } yield keepAlive(request)

    lazy val cont: IO[Exception, Unit] = for {
      keep <- base
      _ <- Log.debug(logQueue)("Worker: keep-alive? " + keep)
      next = if (keep) cont else IO.unit
      _ <- next
    } yield ()

    cont.ensuring(WebIO.close(socket))
        .catchAll(Log.exception(logQueue))
  }

  def getRequest(socket: Socket): IO[Exception, Option[\/[HttpError, Request]]] =
    Log.debug(logQueue)("Worker: getting request") *>
    WebIO.getRequest(socket).map(Some(_))
                            .catchAll(_ => Log.debug(logQueue)("Worker: timeout") *>
                                           IO.succeedLazy(Option.empty[\/[HttpError, Request]]))

  def send(socket: Socket, response: Response): IO[Exception, Any] =
    Log.debug(logQueue)("Worker: sending response") *>
    WebIO.sendResponse(socket, response)

  def getResponse(optRequest: Option[\/[HttpError, Request]]): IO[Exception, Response] =
    Log.debug(logQueue)("Worker: getting response") *> (
    optRequest match {
      case Some(evr) => evr match {
        case \/-(r) => resolver.resolve(r.path, pathTree) match {
          case \/-(v) => v(r)
          case -\/(error) => IO.succeedLazy(HttpErrorResponse(error, content = error.toString))
        }
        case -\/(error) => IO.succeedLazy(HttpErrorResponse(error, content = error.toString))
      }
      case None => IO.succeedLazy(HttpErrorResponse(Http408, Http408.toString))
    })

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
