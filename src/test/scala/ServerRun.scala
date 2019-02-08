import java.io.InputStream
import java.net.Socket

import org.scalatest._
import pl.edu.mimuw.sws.UrlResolver.Controller
import pl.edu.mimuw.sws._
import scalaz.\/
import scalaz.zio._
import scalaz.zio.RTS
import scalaz.zio.duration._

import scala.annotation.tailrec
import scala.language.postfixOps

class ServerRun extends FlatSpec {
  private final val rts: RTS = new RTS{}

  def testServer(testconf: String, tester: IO[Unit, Boolean]): Boolean = {
    def dummy(request: Request, args: Map[String, String]): Response = HttpResponse("")
    def http204Status(request: Request, args: Map[String, String]): Response = HttpResponse("", statusCode = Http204)
    def redirect(request: Request, args: Map[String, String]): Response = HttpRedirectResponse("http://www.google.com")
    def cookie(request: Request, args: Map[String, String]): Response =
      HttpResponse("", cookies = List(Cookie("key", "value", httpOnly = false)))
    def parseGet(request: Request, args: Map[String, String]): Response =
      HttpResponse(request.query.getOrElse("key", "none"))
    def parseArgs(request: Request, args: Map[String, String]): Response =
      HttpResponse(args.getOrElse("key", "none"))

    val testUrls: List[(String, Controller)] = List(
      ("", dummy),
      ("/204", http204Status),
      ("/redirect", redirect),
      ("/cookie", cookie),
      ("/get", parseGet),
      ("/args/<key>", parseArgs)
    )

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

  private def readUntilDoubleCRLF(is: InputStream): Option[(String, List[Byte])] = {
    @tailrec
    def splitInput(left: List[Byte], right: List[Byte]): (List[Byte], List[Byte], Boolean) = {
      import pl.edu.mimuw.sws.Request.HttpConstants.{CR, LF}

      right match {
        case h :: hr => left match {
          case l1 :: l2 :: l3 :: _ if h == LF && l1 == CR && l2 == LF && l3 == CR =>
            (h :: left, hr, true)
          case _ => splitInput(h :: left, hr)
        }
        case Nil =>
          (left, Nil, false)
      }
    }

    val buffer = new Array[Byte](1024)

    @tailrec
    def readStringFromSocket(rest: List[Byte]): Option[(String, List[Byte])] = {
      is.read(buffer) match {
        case -1 | 0 => None
        case count: Int =>
          val (str, bytes, split) = splitInput(rest, buffer.take(count).toList)
          if (split) Some((str.reverse.map(_.toChar).mkString, bytes)) else readStringFromSocket(str)
      }
    }

    readStringFromSocket(Nil)
  }

  private def readBody(is: InputStream, bodyStart: List[Byte], length: Int): List[Byte] = {
    @tailrec
    def readFromSocket(left: Int, read: List[Byte]): List[Byte] = {
      if (left <= 0) Nil else readFromSocket(left - 1, is.read.toByte :: read)
    }

    val bodyStartLen = bodyStart.length
    val bodyRest = readFromSocket(length - bodyStartLen, Nil)

    bodyStart ::: bodyRest
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

  it should "not accept invalid request method" in {
    val configFile = "testing/test3.conf"

    val tester: IO[Unit, Boolean] = for {
      port <- ServerDataReader.readConfigFile(configFile).map(sd => Some(sd.port))
        .catchAll(_ => IO.succeedLazy(Option.empty[Int]))
      socket <- port match {
        case Some(p) => WebIO.connectTo("localhost", p).map(Some(_))
          .catchAll(_ => IO.succeedLazy(Option.empty[Socket]))
        case None => IO.succeedLazy(Option.empty[Socket])
      }
      sent <- socket match {
        case Some(s) => WebIO.sendT(s, "GOT / HTTP/1.1\r\n\r\n").map(_ => true)
          .catchAll(_ => IO.succeedLazy(false))
        case None => IO.succeedLazy(false)
      }
      success <- socket match {
        case Some(s) => WebIO.getLines(s).map({
          case h :: _ => sent && h == "HTTP/1.1 405 Method Not Allowed"
          case Nil => false
        }).catchAll(_ => IO.succeedLazy(false))
        case None => IO.succeedLazy(false)
      }
    } yield success

    assert(testServer(configFile, tester))
  }

  it should "send 404" in {
    val configFile = "testing/test4.conf"

    val tester: IO[Unit, Boolean] = for {
      port <- ServerDataReader.readConfigFile(configFile).map(sd => Some(sd.port))
        .catchAll(_ => IO.succeedLazy(Option.empty[Int]))
      socket <- port match {
        case Some(p) => WebIO.connectTo("localhost", p).map(Some(_))
          .catchAll(_ => IO.succeedLazy(Option.empty[Socket]))
        case None => IO.succeedLazy(Option.empty[Socket])
      }
      sent <- socket match {
        case Some(s) => WebIO.sendT(s, "GET /wrong_path HTTP/1.1\r\n\r\n").map(_ => true)
          .catchAll(_ => IO.succeedLazy(false))
        case None => IO.succeedLazy(false)
      }
      success <- socket match {
        case Some(s) => WebIO.getLines(s).map({
          case h :: _ => sent && h == "HTTP/1.1 404 Not Found"
          case Nil => false
        }).catchAll(_ => IO.succeedLazy(false))
        case None => IO.succeedLazy(false)
      }
    } yield success

    assert(testServer(configFile, tester))
  }

  it should "not accept newer HTTP protocols" in {
    val configFile = "testing/test5.conf"

    val tester: IO[Unit, Boolean] = for {
      port <- ServerDataReader.readConfigFile(configFile).map(sd => Some(sd.port))
        .catchAll(_ => IO.succeedLazy(Option.empty[Int]))
      socket <- port match {
        case Some(p) => WebIO.connectTo("localhost", p).map(Some(_))
          .catchAll(_ => IO.succeedLazy(Option.empty[Socket]))
        case None => IO.succeedLazy(Option.empty[Socket])
      }
      sent <- socket match {
        case Some(s) => WebIO.sendT(s, "GET / HTTP/2.0\r\n\r\n").map(_ => true)
          .catchAll(_ => IO.succeedLazy(false))
        case None => IO.succeedLazy(false)
      }
      success <- socket match {
        case Some(s) => WebIO.getLines(s).map({
          case h :: _ => sent && h == "HTTP/1.1 505 HTTP Version Not Supported"
          case Nil => false
        }).catchAll(_ => IO.succeedLazy(false))
        case None => IO.succeedLazy(false)
      }
    } yield success

    assert(testServer(configFile, tester))
  }

  it should "not accept wrong number of parameters in first request line" in {
    val configFile = "testing/test6.conf"

    val tester: IO[Unit, Boolean] = for {
      port <- ServerDataReader.readConfigFile(configFile).map(sd => Some(sd.port))
        .catchAll(_ => IO.succeedLazy(Option.empty[Int]))
      socket <- port match {
        case Some(p) => WebIO.connectTo("localhost", p).map(Some(_))
          .catchAll(_ => IO.succeedLazy(Option.empty[Socket]))
        case None => IO.succeedLazy(Option.empty[Socket])
      }
      sent <- socket match {
        case Some(s) => WebIO.sendT(s, "GET / too much parameters HTTP/1.1\r\n\r\n").map(_ => true)
          .catchAll(_ => IO.succeedLazy(false))
        case None => IO.succeedLazy(false)
      }
      success <- socket match {
        case Some(s) => WebIO.getLines(s).map({
          case h :: _ => sent && h == "HTTP/1.1 400 Bad Request"
          case Nil => false
        }).catchAll(_ => IO.succeedLazy(false))
        case None => IO.succeedLazy(false)
      }
    } yield success

    assert(testServer(configFile, tester))
  }

  it should "not freeze because of unfinished request" in {
    val configFile = "testing/test7.conf"

    val tester: IO[Unit, Boolean] = for {
      port <- ServerDataReader.readConfigFile(configFile).map(sd => Some(sd.port))
        .catchAll(_ => IO.succeedLazy(Option.empty[Int]))
      socket <- port match {
        case Some(p) => WebIO.connectTo("localhost", p).map(Some(_))
          .catchAll(_ => IO.succeedLazy(Option.empty[Socket]))
        case None => IO.succeedLazy(Option.empty[Socket])
      }
      sent <- socket match {
        case Some(s) => WebIO.sendT(s, "GET / HTTP/1.1\r\n").map(_ => true)
          .catchAll(_ => IO.succeedLazy(false))
        case None => IO.succeedLazy(false)
      }
      success <- socket match {
        case Some(s) => WebIO.getLines(s).map({
          case h :: _ => sent && h == "HTTP/1.1 408 Request Timeout"
          case Nil => false
        }).catchAll(_ => IO.succeedLazy(false))
        case None => IO.succeedLazy(false)
      }
    } yield success

    assert(testServer(configFile, tester))
  }

  it should "not freeze because of wrong content length" in {
    val configFile = "testing/test8.conf"

    val tester: IO[Unit, Boolean] = for {
      port <- ServerDataReader.readConfigFile(configFile).map(sd => Some(sd.port))
        .catchAll(_ => IO.succeedLazy(Option.empty[Int]))
      socket <- port match {
        case Some(p) => WebIO.connectTo("localhost", p).map(Some(_))
          .catchAll(_ => IO.succeedLazy(Option.empty[Socket]))
        case None => IO.succeedLazy(Option.empty[Socket])
      }
      sent <- socket match {
        case Some(s) => WebIO.sendT(s, "POST / HTTP/1.1\r\nContent-Length: 100\r\n\r\ntooshortbody").map(_ => true)
          .catchAll(_ => IO.succeedLazy(false))
        case None => IO.succeedLazy(false)
      }
      success <- socket match {
        case Some(s) => WebIO.getLines(s).map({
          case h :: _ => sent && h == "HTTP/1.1 408 Request Timeout"
          case Nil => false
        }).catchAll(_ => IO.succeedLazy(false))
        case None => IO.succeedLazy(false)
      }
    } yield success

    assert(testServer(configFile, tester))
  }

  it should "process over request that should not have body and has wrong content length" in {
    val configFile = "testing/test9.conf"

    val tester: IO[Unit, Boolean] = for {
      port <- ServerDataReader.readConfigFile(configFile).map(sd => Some(sd.port))
        .catchAll(_ => IO.succeedLazy(Option.empty[Int]))
      socket <- port match {
        case Some(p) => WebIO.connectTo("localhost", p).map(Some(_))
          .catchAll(_ => IO.succeedLazy(Option.empty[Socket]))
        case None => IO.succeedLazy(Option.empty[Socket])
      }
      sent <- socket match {
        case Some(s) => WebIO.sendT(s, "DELETE / HTTP/1.1\r\nContent-Length: 100\r\n\r\nthis is my body").map(_ => true)
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

  it should "send other statuses if specified" in {
    val configFile = "testing/test10.conf"

    val tester: IO[Unit, Boolean] = for {
      port <- ServerDataReader.readConfigFile(configFile).map(sd => Some(sd.port))
        .catchAll(_ => IO.succeedLazy(Option.empty[Int]))
      socket <- port match {
        case Some(p) => WebIO.connectTo("localhost", p).map(Some(_))
          .catchAll(_ => IO.succeedLazy(Option.empty[Socket]))
        case None => IO.succeedLazy(Option.empty[Socket])
      }
      sent <- socket match {
        case Some(s) => WebIO.sendT(s, "GET /204 HTTP/1.1\r\n\r\n").map(_ => true)
          .catchAll(_ => IO.succeedLazy(false))
        case None => IO.succeedLazy(false)
      }
      success <- socket match {
        case Some(s) => WebIO.getLines(s).map({
          case h :: _ => sent && h == "HTTP/1.1 204 No Content"
          case Nil => false
        }).catchAll(_ => IO.succeedLazy(false))
        case None => IO.succeedLazy(false)
      }
    } yield success

    assert(testServer(configFile, tester))
  }

  it should "redirect" in {
    val configFile = "testing/test11.conf"

    val tester: IO[Unit, Boolean] = for {
      port <- ServerDataReader.readConfigFile(configFile).map(sd => Some(sd.port))
        .catchAll(_ => IO.succeedLazy(Option.empty[Int]))
      socket <- port match {
        case Some(p) => WebIO.connectTo("localhost", p).map(Some(_))
          .catchAll(_ => IO.succeedLazy(Option.empty[Socket]))
        case None => IO.succeedLazy(Option.empty[Socket])
      }
      sent <- socket match {
        case Some(s) => WebIO.sendT(s, "GET /redirect HTTP/1.1\r\n\r\n").map(_ => true)
          .catchAll(_ => IO.succeedLazy(false))
        case None => IO.succeedLazy(false)
      }
      success <- socket match {
        case Some(s) => WebIO.getLines(s).map({
          case h :: h2 :: _ => sent && h == "HTTP/1.1 302 Found" && h2 == "Location: http://www.google.com"
          case Nil | _ => false
        }).catchAll(_ => IO.succeedLazy(false))
        case None => IO.succeedLazy(false)
      }
    } yield success

    assert(testServer(configFile, tester))
  }

  it should "set cookie" in {
    val configFile = "testing/test12.conf"

    val tester: IO[Unit, Boolean] = for {
      port <- ServerDataReader.readConfigFile(configFile).map(sd => Some(sd.port))
        .catchAll(_ => IO.succeedLazy(Option.empty[Int]))
      socket <- port match {
        case Some(p) => WebIO.connectTo("localhost", p).map(Some(_))
          .catchAll(_ => IO.succeedLazy(Option.empty[Socket]))
        case None => IO.succeedLazy(Option.empty[Socket])
      }
      sent <- socket match {
        case Some(s) => WebIO.sendT(s, "GET /cookie HTTP/1.1\r\n\r\n").map(_ => true)
          .catchAll(_ => IO.succeedLazy(false))
        case None => IO.succeedLazy(false)
      }
      success <- socket match {
        case Some(s) => WebIO.getLines(s).map({
          case h :: t => sent && h == "HTTP/1.1 200 OK" && t.contains("Set-Cookie: key=value")
          case Nil | _ => false
        }).catchAll(_ => IO.succeedLazy(false))
        case None => IO.succeedLazy(false)
      }
    } yield success

    assert(testServer(configFile, tester))
  }

  it should "parse get query arguments" in {
    val configFile = "testing/test13.conf"

    val tester: IO[Unit, Boolean] = for {
      port <- ServerDataReader.readConfigFile(configFile).map(sd => Some(sd.port))
        .catchAll(_ => IO.succeedLazy(Option.empty[Int]))
      socket <- port match {
        case Some(p) => WebIO.connectTo("localhost", p).map(Some(_))
          .catchAll(_ => IO.succeedLazy(Option.empty[Socket]))
        case None => IO.succeedLazy(Option.empty[Socket])
      }
      sent <- socket match {
        case Some(s) => WebIO.sendT(s, "GET /get?key=value HTTP/1.1\r\n\r\n").map(_ => true)
          .catchAll(_ => IO.succeedLazy(false))
        case None => IO.succeedLazy(false)
      }
      success <- socket match {
        case Some(s) if sent => WebIO.getInputStream(s).map(is => {
          readUntilDoubleCRLF(is).exists({
            case (_, bodyStart) => readBody(is, bodyStart, 5).map(_.toChar).mkString == "value"
            case _ => false
          })
        }).catchAll(_ => IO.succeedLazy(false))
        case _ => IO.succeedLazy(false)
      }
    } yield success

    assert(testServer(configFile, tester))
  }

  it should "parse dynamic arguments in path" in {
    val configFile = "testing/test14.conf"

    val tester: IO[Unit, Boolean] = for {
      port <- ServerDataReader.readConfigFile(configFile).map(sd => Some(sd.port))
        .catchAll(_ => IO.succeedLazy(Option.empty[Int]))
      socket <- port match {
        case Some(p) => WebIO.connectTo("localhost", p).map(Some(_))
          .catchAll(_ => IO.succeedLazy(Option.empty[Socket]))
        case None => IO.succeedLazy(Option.empty[Socket])
      }
      sent <- socket match {
        case Some(s) => WebIO.sendT(s, "GET /args/value HTTP/1.1\r\n\r\n").map(_ => true)
          .catchAll(_ => IO.succeedLazy(false))
        case None => IO.succeedLazy(false)
      }
      success <- socket match {
        case Some(s) if sent => WebIO.getInputStream(s).map(is => {
          readUntilDoubleCRLF(is).exists({
            case (_, bodyStart) => readBody(is, bodyStart, 5).map(_.toChar).mkString == "value"
            case _ => false
          })
        }).catchAll(_ => IO.succeedLazy(false))
        case _ => IO.succeedLazy(false)
      }
    } yield success

    assert(testServer(configFile, tester))
  }
}
