package pl.edu.mimuw.sws

import java.io.{BufferedReader, InputStreamReader}

import scala.annotation.tailrec
import scala.collection.immutable.Map  // explicit immutability
import scalaz._
import Scalaz._

// TODO: Hide this somehow (private class shows warnings)
class Request private(environ: Map[String, String]) {
  def response: String = {
    val body = "<html><head><title>Test Page</title></head><body>" + environ.show + "</body></html>"
    "HTTP/1.1 200 OK\r\n" + "Content-Type: text/html; charset=UTF-8\r\n" +
    "Content-Length: " + body.length + "\r\n" +
    "Connection: close\r\n" + "\r\n" + body
  }
}

object Request {
  // At first it was called parseRequest, but `apply` seems to be
  // a syntactic sugar we could use
  def apply(socket: java.net.Socket): Request = {
    // TODO: Probably we should wrap it into some intelligent side-effect handler
    val in = new BufferedReader(new InputStreamReader(socket.getInputStream))

    val parsedRequestLine = parseRequestLine(in.readLine())

    val environ = parseHeaders(in, parsedRequestLine)

    new Request(environ)
  }

  private def parseRequestLine(requestLine: String): Map[String, String] = {
    val Array(method, path, _*) = requestLine.split(" ")
    Map("REQUEST_METHOD" -> method, "PATH_INFO" -> path)
  }

  @tailrec
  private def parseHeaders(reader: BufferedReader,
                           environ: Map[String, String]): Map[String, String] = {
    val line = reader.readLine()

    if (line != null && line != "") {
      val updatedEnviron = environ |+| (line.split(": ") match {
        case Array(key, value) => Map(key -> value)
        case _ => Monoid[Map[String, String]].zero
      })

      parseHeaders(reader, updatedEnviron)
    }
    else {
      environ
    }
  }
}
