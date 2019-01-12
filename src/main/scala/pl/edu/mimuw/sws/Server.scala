package pl.edu.mimuw.sws
import scalaz.zio._


case class Server (configFile: String) {
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
    worker = Worker(serverDataRef, logQueue)

    acceptAndFork = for {
      socket <- WebIO.accept(serverSocket)
      _ <- worker.talk(socket).fork
    } yield ()

    // continuously accept
    _ <- acceptAndFork.forever.catchAll(_ => IO.unit)

  } yield ()
}