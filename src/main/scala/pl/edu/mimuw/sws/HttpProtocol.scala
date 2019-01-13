package pl.edu.mimuw.sws

import scalaz._
import Scalaz._

sealed trait HttpProtocol

object HttpProtocol {

  case object Http1_0 extends HttpProtocol

  case object Http1_1 extends HttpProtocol

  def apply(protocol: String): \/[HttpError, HttpProtocol] = protocol match {
    case "HTTP/1.0" => Http1_0.right
    case "HTTP/1.1" => Http1_1.right
    case _ => Http505.left
  }
}