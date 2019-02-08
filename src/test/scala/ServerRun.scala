import java.net.Socket

import org.scalatest._
import pl.edu.mimuw.sws._
import pl.edu.mimuw.testapi.TestMain
import scalaz.zio._
import scalaz.zio.RTS
import scalaz.zio.duration._

import scala.language.postfixOps

class ServerRun extends FlatSpec {
  private final val rts: RTS = new RTS{}

  def testServer(testconf: String, tester: IO[Unit, Boolean]): Boolean = {
    val server = Server(testconf,
      TestMain.urls,
      TestMain.urlsIO,
      TestMain.static,
      TestMain.favicon)
    rts.unsafeRun(
      for {
        serverFiber <- server.run.fork
        _ <- IO.sleep(100 microseconds) // wait for server to start
        success <- tester // run testing
        _ <- serverFiber.interrupt // interrupt server fiber
      } yield success
    )
  }

  "The server" should "run" in {
    val tester: IO[Unit, Boolean] = for {
      success <- IO.succeedLazy(true)
    } yield success

    assert(testServer("testing/test0.conf", tester))
  }

  it should "receive clients" in {
    val tester: IO[Unit, Boolean] = for {
      socket <- WebIO.connectTo("localhost", 9991).map(Some(_))
        .catchAll(_ => IO.succeedLazy(Option.empty[Socket]))
      success = socket.nonEmpty
    } yield success

    assert(testServer("testing/test1.conf", tester))
  }
}
