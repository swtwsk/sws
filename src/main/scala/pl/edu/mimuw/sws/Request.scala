package pl.edu.mimuw.sws

import java.io.{BufferedReader, InputStreamReader}

import scala.annotation.tailrec
import scala.collection.immutable.Map  // explicit immutability
import scalaz._
import Scalaz._

// TODO: Hide this somehow (private class shows warnings)
// TODO: Maybe we should pack query into special Query class?
case class Request private(method: String,
                           path: String,
                           environ: Map[String, String],
                           query: Map[String, String]) {
  def body: String = "<html><head><title>Test Page</title></head><body>" +
        "<div><b>Method: </b>" + method + "</div>" +
        "<div><b>Path: </b>" + path + "</div>" +
        "<div><b>Environ: </b>" + environ.show + "</div>" +
        "<div><b>Query: </b>" + query.show + "</div>" +
        "</body></html>"
}

object Request {
  // At first it was called parseRequest, but `apply` seems to be
  // a syntactic sugar we could use
  def apply(socket: java.net.Socket): Option[Request] = {
    // TODO: Probably we should wrap it into some intelligent side-effect handler
    val in = new BufferedReader(new InputStreamReader(socket.getInputStream))

    val parsedRequestLine = parseRequestLine(in.readLine())
    parsedRequestLine match {
      case Some((method, pathInfo)) => {
        val environ = parseHeaders(in, Map())
        val (path, query) = extractQuery(pathInfo)

        new Request(method, path, environ, query).some
      }
      case None => none[Request]
    }
  }

  private def parseRequestLine(requestLine: String): Option[(String, String)] = {
    requestLine.split(" ") match {
      case Array(method, path, _*) => (method, path).some
      case _ => none
    }
  }

  @tailrec
  private def parseHeaders(reader: BufferedReader,
                           environ: Map[String, String]): Map[String, String] = {
    val line = reader.readLine()

    if (line != null && line != "") {
      val updatedEnviron = environ |+| (line.split(": ") match {
        case Array(key, value) => Map(key -> value)
        case _ => Map()
      })

      parseHeaders(reader, updatedEnviron)
    }
    else {
      environ
    }
  }

  private def extractQuery(pathInfo: String): (String, Map[String, String]) = {
    import java.net.URLDecoder

    val (path, queryString) = pathInfo.split("\\?") match {
      case Array(p, qs, _*) => (p, qs)
      case Array(p) => (p, "")
    }

    // based on https://gist.github.com/gvolpe/6f91d905ed94136a2198
    val queryMap = queryString.split("&").flatMap(q => {
      val m = q.split("=", 2).map(s => URLDecoder.decode(s, "UTF-8"))
      m match {
        case Array(k, v) => (k -> v).some
        case _ => none[(String, String)]
      }
    }).toMap

    (path, queryMap)
  }
}
