import org.scalatest._
import pl.edu.mimuw.sws._
import pl.edu.mimuw.testapi.TestMain
import scalaz.zio._
import scalaz.zio.console._
import scalaz.zio.RTS
import scalaz.zio.duration._
import scala.language.postfixOps

class ServerRun extends FlatSpec {
  private final val rts: RTS = new RTS{}
  private final val server = Server(TestMain.defaultConfigFile,
                                     TestMain.urls,
                                     TestMain.urlsIO,
                                     TestMain.static,
                                     TestMain.favicon)
  "The server" should "run" in {
        val testing: IO[Unit, Any] = for {
          _ <- putStrLn("Some testing...")
        } yield ()

        rts.unsafeRun(for {
            serverFiber <- server.run.fork
            _ <- IO.sleep(100 microseconds) // wait for server to start
            _ <- testing                               // run testing
            _ <- serverFiber.interrupt                 // interrupt server fiber
        } yield ())

        assert(true)
  }
}
