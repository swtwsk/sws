package pl.edu.mimuw.sws

import scalaz._
import Scalaz._

sealed trait HttpMethod {
  def name: String
  def isSafe: Boolean
  def isIdempotent: Boolean

  override def toString: String = "Http method: " + name
}

object HttpMethod {
  case object Get extends HttpMethod {
    val name = "GET"
    val isSafe = true
    val isIdempotent = true
  }

  case object Post extends HttpMethod {
    val name = "POST"
    val isSafe = false
    val isIdempotent = false
  }

  case object Option extends HttpMethod {
    val name = "OPTION"
    val isSafe = true
    val isIdempotent = true
  }

  val methods: Map[String, HttpMethod] = Map("GET" -> Get, "POST" -> Post)

  def apply(name: String): \/[HttpError, HttpMethod] = methods.get(name) match {
    case Some(method) => method.right
    case None => Http405.left
  }
}
