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
        rts.unsafeRun(
            server.run.fork.flatMap(
              f => IO.sleep(1.second) *> putStrLn("Interrupt")
                                      *> f.interrupt
                                      *> putStrLn("Interrupted")
            )
        )
        rts.shutdown()
        assert(true)
  }
}
