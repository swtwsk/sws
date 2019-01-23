package pl.edu.mimuw.sws
import scalaz.zio._
import scalaz.zio.duration._


case class Configurer (serverDataRef: Ref[ServerData], logQueue: Queue[Log], configFile: String) {
  val getServerData: IO[Nothing, ServerData] =
    ServerDataReader.readConfigFile(configFile)
      .catchAll(e => Log.warning(logQueue)("Using default config")
                  *> IO.point(ServerData.default))

  val refresh: IO[Nothing, Unit] = for {
    serverData <- getServerData
    _ <- serverDataRef.setLater(serverData)
  } yield ()

  val run: IO[Nothing, Unit] =
    refresh.delay(10.seconds).forever
}
