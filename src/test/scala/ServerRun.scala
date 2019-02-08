import java.net.Socket

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
  private final val server = Server("testing/test.conf",
                                     TestMain.urls,
                                     TestMain.urlsIO,
                                     TestMain.static,
                                     TestMain.favicon)

  def testServer(tester: IO[Unit, Boolean]): Boolean =
        rts.unsafeRun(
          for {
            serverFiber <- server.run.fork
            _ <- putStrLn("readyy for nap time")
            _ <- IO.sleep(10 seconds) // wait for server to start
            _ <- putStrLn("done")
            success <- tester                          // run testing
            _ <- serverFiber.interrupt                 // interrupt server fiber
          } yield success
        )

  "The server" should "run" in {
        val tester: IO[Unit, Boolean] = for {
          _ <- putStrLn("Some testing...")
          success <- IO.succeedLazy(true)
        } yield success

        assert(testServer(tester))
  }

  it should "receive clients" in {
        val tester: IO[Unit, Boolean] = for {
          socket <- WebIO.connectTo("localhost", 9999).map(Some(_))
                         .catchAll(_ => IO.succeedLazy(Option.empty[Socket]))
          success = socket.nonEmpty
        } yield success

        assert(testServer(tester))
  }
}
