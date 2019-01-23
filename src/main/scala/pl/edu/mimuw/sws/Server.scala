package pl.edu.mimuw.sws
import pl.edu.mimuw.sws.UrlResolver.{Controller, IOController}
import scalaz.zio._


case class Server(configFile: String,
                  urls: List[(String, Controller)],
                  urlsIO: List[(String, IOController)],
                  static: Option[(String, String)],
                  favicon: Option[String]) {
  val run: IO[Nothing, Unit] = for {

    // create queue for logger
    logQueue <- Queue.unbounded[Log]

    // read server data from config file
    serverData <- ServerDataReader.readConfigFile(configFile)
                                  .catchAll(e => Log.warning(logQueue)("Using default config")
                                              *> IO.point(ServerData.default))

    // debug info
    _ <- Log.debug(logQueue)("Server port: " + serverData.port)

    // create reference to hold server data
    serverDataRef <- Ref(serverData)

    // start logger
    logger = new Logger(serverDataRef, logQueue)
    _ <- logger.run.fork

    // start configurer
    configurer = Configurer(serverDataRef, logQueue, configFile)
    _ <- configurer.run.fork

    _ <- Log.debug(logQueue)("Server: starting to listen")

    // open ServerSocket
    serverSocket <- Combinators.insist(WebIO.listenOn(serverData))(Log.exception(logQueue))

    // init. worker
    pathTree = PathNode(urls, urlsIO)
    resolver = UrlResolver(static, favicon)
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
