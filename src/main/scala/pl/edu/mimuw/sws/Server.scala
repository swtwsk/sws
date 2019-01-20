package pl.edu.mimuw.sws
import scalaz.zio._


case class Server (configFile: String) {
  val run: IO[Nothing, Unit] = for {

    // create queue for logger
    logQueue <- Queue.unbounded[Log]

    // read server data from config file
    serverData <- ServerDataReader.readConfigFile(configFile)
                                  .catchAll(e => Log.to(logQueue)(e) *> IO.point(ServerData.default))

    // create reference to hold server data
    serverDataRef <- Ref(serverData)

    // start logger
    loggerFiber <- Logger(serverDataRef, logQueue).log.forever.fork

    // open ServerSocket
    serverSocket <- Combinators.insist(WebIO.listenOn(serverData))(Log.to(logQueue))

    // init. worker
    worker = Worker(serverDataRef, logQueue)

    acceptAndFork = for {
      socket <- Combinators.insist(WebIO.accept(serverSocket))(Log.to(logQueue))
      _ <- worker.talk(socket).fork
    } yield ()

    // continuously accept
    _ <- acceptAndFork.forever

  } yield ()
}
