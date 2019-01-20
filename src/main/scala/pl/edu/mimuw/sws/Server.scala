package pl.edu.mimuw.sws
import pl.edu.mimuw.sws.UrlResolver.{PathNode, View}
import scalaz.zio._


case class Server (configFile: String, urls: List[(String, View)]) {
  val run: IO[Nothing, Unit] = for {

    // read server data from config file
    serverData <- ServerDataReader.readConfigFile(configFile)

    // create reference to hold server data
    serverDataRef <- Ref(serverData)

    // create queue for logger
    logQueue <- Queue.unbounded[Log]

    // open ServerSocket
    serverSocket <- WebIO.listenOn(serverData)

    // start logger
    loggerFiber <- Logger(serverDataRef, logQueue).log.forever.fork

    // init. worker
    worker = Worker(serverDataRef, logQueue, PathNode(urls))

    acceptAndFork = for {
      socket <- WebIO.accept(serverSocket)
      _ <- worker.talk(socket).fork
    } yield ()

    // continuously accept
    _ <- acceptAndFork.forever.catchAll(_ => IO.unit)

  } yield ()
}