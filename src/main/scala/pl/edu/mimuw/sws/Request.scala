package pl.edu.mimuw.sws
import java.io.InputStream

import scala.annotation.tailrec
import scala.collection.immutable.Map
import scalaz._
import Scalaz._

// TODO: Maybe we should pack query into special Query class?
// TODO: What about protocol?
case class Request private(method: String,
                           path: String,
                           protocol: String,
                           environ: Map[String, String],
                           query: Map[String, String],
                           body: List[Byte]) {
  def responseBody: String = "<html><head><title>Test Page</title></head><body>" +
    "<div><b>Method: </b>" + method + "</div>" +
    "<div><b>Path: </b>" + path + "</div>" +
    "<div><b>Environ: </b>" + environ.show + "</div>" +
    "<div><b>Query: </b>" + query.show + "</div>" +
    "<div><b>Body: </b>" + body.show + "</div>" +
    "</body></html>"
}

object Request {
  object HttpConstants {
    val CR: Byte = '\r'.toByte
    val LF: Byte = '\n'.toByte
  }

  def apply(socket: java.net.Socket): Option[Request] = {
    val in: InputStream = socket.getInputStream

    readUntilDoubleCRLF(in) match {
      case Some((headers, bodyStart)) =>
        val headersList = headers.split("\r\n").toList
        parseRequestLine(headersList.headOption.getOrElse("")) match {
          case Some((method, pathInfo, protocol)) =>
            val environ = parseHeaders(headersList.tailOption.getOrElse(Nil))
            val (path, query) = extractQuery(pathInfo)
            val body = readBody(in, bodyStart, environ.getOrElse("Content-Length", "0").parseInt.toOption.getOrElse(0))

            new Request(method, path, protocol, environ, query, body).some
          case None => none
        }
      case None => none
    }
  }

  private def readUntilDoubleCRLF(is: InputStream): Option[(String, List[Byte])] = {
    @tailrec
    def splitInput(left: List[Byte], right: List[Byte]): (List[Byte], List[Byte], Boolean) = {
      import HttpConstants.{CR, LF}

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
        case 0 => none
        case count: Int =>
          val (str, bytes, split) = splitInput(rest, buffer.take(count).toList)
          if (split) (str.reverse.map(_.toChar).mkString, bytes).some else readStringFromSocket(str)
      }
    }

    readStringFromSocket(Nil)
  }

  // TODO: We should also parse chunked transfer encoding
  //  https://en.wikipedia.org/wiki/Chunked_transfer_encoding
  private def readBody(is: InputStream, bodyStart: List[Byte], length: Int): List[Byte] = {
    @tailrec
    def readFromSocket(left: Int, read: List[Byte]): List[Byte] = {
      if (left <= 0) Nil else readFromSocket(left - 1, is.read.toByte :: read)
    }

    val bodyStartLen = bodyStart.length
    val bodyRest = readFromSocket(length - bodyStartLen, Nil)

    bodyStart ::: bodyRest
  }

  private def parseRequestLine(requestLine: String): Option[(String, String, String)] = {
    requestLine.split(" ") match {
      case Array(method, path, protocol) => (method, path, protocol).some
      case _ => none
    }
  }

  private def parseHeaders(stringList: List[String]): Map[String, String] = {
    @tailrec
    def iter(lines: List[String], keys: List[Array[String]]): List[Array[String]] = lines match {
      case "" :: _ => keys
      case line :: lr => iter(lr, line.split(": ") :: keys)
      case Nil => keys
    }

    iter(stringList, List()).flatMap(_ match {
      case Array(k, v) => (k -> v).some
      case _ => none[(String, String)]
    }).toMap
  }

  private def extractQuery(pathInfo: String): (String, Map[String, String]) = {
    import java.net.URLDecoder

    val (path, queryString) = pathInfo.split("\\?") match {
      case Array(p, qs, _*) => (p, qs)
      case Array(p) => (p, "")
    }

    // based on https://gist.github.com/gvolpe/6f91d905ed94136a2198
    val queryMap = queryString.split("&").flatMap(q => {
      val m = q.split("=").map(s => URLDecoder.decode(s, "UTF-8"))
      m match {
        case Array(k, v) => (k -> v).some
        case _ => none[(String, String)]
      }
    }).toMap

    (path, queryMap)
  }
}
