package pl.edu.mimuw.sws
import scalaz.zio._
import java.net.Socket
import java.util.concurrent.TimeUnit
import scalaz.zio.duration.Duration


case class Worker(serverData: Ref[ServerData], logQueue: Queue[Log]) {
  def talk(socket: Socket): IO[Nothing, Unit] =
    getRequest(socket).flatMap(getResponse).flatMap(WebIO.send(socket, _))
                      .ensuring(WebIO.close(socket))
                      .catchAll(Log.to(logQueue))

  def getRequest(socket: Socket): IO[Exception, Option[Request]] =
    WebIO.getRequest(socket).timeout0(Option.empty[Request])(identity)(Duration(5, TimeUnit.SECONDS))

  def getResponse(optRequest: Option[Request]): IO[Exception, HttpResponse] = IO.syncException(
    optRequest match  {
      case Some(r) => HttpResponse(r.body)
      case None => HttpResponse("", statusCode = Http400)
    }
  )
}
