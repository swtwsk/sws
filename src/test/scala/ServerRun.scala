import java.net.Socket

import org.scalatest._
import pl.edu.mimuw.sws.UrlResolver.Controller
import pl.edu.mimuw.sws._
import scalaz.zio._
import scalaz.zio.RTS
import scalaz.zio.duration._

import scala.language.postfixOps

class ServerRun extends FlatSpec {
  private final val rts: RTS = new RTS{}

  def testServer(testconf: String, tester: IO[Unit, Boolean]): Boolean = {
    def dummy(request: Request, args: Map[String, String]): Response = HttpResponse("")
    val testUrls: List[(String, Controller)] = List(("", dummy))

    val server = Server(testconf, testUrls, List(), None, None)

    rts.unsafeRun(
      for {
        serverFiber <- server.run.fork
        _ <- IO.sleep(1 second) // wait for server to start
        success <- tester // run testing
        _ <- serverFiber.interrupt // interrupt server fiber
      } yield success
    )
  }

  "The server" should "run" in {
    val configFile = "testing/test0.conf"

    val tester: IO[Unit, Boolean] = for {
      success <- IO.succeedLazy(true)
    } yield success

    assert(testServer(configFile, tester))
  }

  it should "receive clients" in {
    val configFile = "testing/test1.conf"

    val tester: IO[Unit, Boolean] = for {
      port <- ServerDataReader.readConfigFile(configFile).map(sd => Some(sd.port))
        .catchAll(_ => IO.succeedLazy(Option.empty[Int]))
      socket <- port match {
        case Some(p) => WebIO.connectTo("localhost", p).map(Some(_))
          .catchAll(_ => IO.succeedLazy(Option.empty[Socket]))
        case None => IO.succeedLazy(Option.empty[Socket])
      }
      success = socket.nonEmpty
    } yield success

    assert(testServer(configFile, tester))
  }

  it should "send 200 OK status" in {
    val configFile = "testing/test2.conf"

    val tester: IO[Unit, Boolean] = for {
      port <- ServerDataReader.readConfigFile(configFile).map(sd => Some(sd.port))
        .catchAll(_ => IO.succeedLazy(Option.empty[Int]))
      socket <- port match {
        case Some(p) => WebIO.connectTo("localhost", p).map(Some(_))
          .catchAll(_ => IO.succeedLazy(Option.empty[Socket]))
        case None => IO.succeedLazy(Option.empty[Socket])
      }
      sent <- socket match {
        case Some(s) => WebIO.sendT(s, "GET / HTTP/1.1\r\n\r\n").map(_ => true)
          .catchAll(_ => IO.succeedLazy(false))
        case None => IO.succeedLazy(false)
      }
      success <- socket match {
        case Some(s) => WebIO.getLines(s).map({
          case h :: _ => sent && h == "HTTP/1.1 200 OK"
          case Nil => false
        }).catchAll(_ => IO.succeedLazy(false))
        case None => IO.succeedLazy(false)
      }
    } yield success

    assert(testServer(configFile, tester))
  }
}
