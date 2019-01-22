package pl.edu.mimuw.sws
import pl.edu.mimuw.sws.UrlResolver.{Controller, IOController}
import scalaz.zio._


case class Server(configFile: String,
                  urls: List[(String, Controller)],
                  urlsIO: List[(String, IOController)],
                  static: Option[(String, String)]) {
  val run: IO[Nothing, Unit] = for {

    // create queue for logger
    logQueue <- Queue.unbounded[Log]

    // read server data from config file
    serverData <- ServerDataReader.readConfigFile(configFile)
                                  .catchAll(e => Log.exception(logQueue)(e) *> IO.point(ServerData.default))

    // create reference to hold server data
    serverDataRef <- Ref(serverData)

    // start logger
    loggerFiber <- Logger(serverDataRef, logQueue, Severity.Debug).log.forever.fork

    // open ServerSocket
    serverSocket <- Combinators.insist(WebIO.listenOn(serverData))(Log.exception(logQueue))

    pathTree = PathNode(urls, urlsIO)
    resolver = UrlResolver(static)
    // init. worker
    worker = Worker(serverDataRef, logQueue, pathTree, resolver)

    acceptAndFork = for {
      socket <- Combinators.insist(WebIO.accept(serverSocket))(Log.exception(logQueue))
      _ <- Log.debug(logQueue)("Server: new connection")
      _ <- worker.talk(socket).fork
    } yield ()

    // continuously accept
    _ <- acceptAndFork.forever

  } yield ()
}
