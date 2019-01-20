package pl.edu.mimuw.sws

import scalaz._
import Scalaz._

sealed trait HttpMethod {
  def name: String
  def isSafe: Boolean
  def isIdempotent: Boolean
  def hasBody: Boolean

  override def toString: String = name
}

object HttpMethod {
  case object Get extends HttpMethod {
    val name = "GET"
    val isSafe = true
    val isIdempotent = true
    val hasBody = true  // "optional" -> enum?
  }

  case object Head extends HttpMethod {
    val name = "HEAD"
    val isSafe = true
    val isIdempotent = true
    val hasBody = false
  }

  case object Post extends HttpMethod {
    val name = "POST"
    val isSafe = false
    val isIdempotent = false
    val hasBody = true
  }

  case object Put extends HttpMethod {
    val name = "PUT"
    val isSafe = false
    val isIdempotent = true
    val hasBody = true
  }

  case object Delete extends HttpMethod {
    val name = "DELETE"
    val isSafe = false
    val isIdempotent = true
    val hasBody = false
  }

  case object Connect extends HttpMethod {
    val name = "CONNECT"
    val isSafe = false
    val isIdempotent = false
    val hasBody = true
  }

  case object Options extends HttpMethod {
    val name = "OPTIONS"
    val isSafe = true
    val isIdempotent = true
    val hasBody = true // Optional, once again
  }

  case object Trace extends HttpMethod {
    val name = "TRACE"
    val isSafe = true
    val isIdempotent = true
    val hasBody = false
  }

  case object Patch extends HttpMethod {
    val name = "PATCH"
    val isSafe = false
    val isIdempotent = false
    val hasBody = true
  }

  private val methods: Map[String, HttpMethod] = List(Get, Head, Post, Put, Delete, Connect, Options, Trace, Patch)
    .map{x => (x.name, x)}
    .toMap

  def apply(name: String): \/[HttpError, HttpMethod] = methods.get(name) match {
    case Some(method) => method.right
    case None => Http405.left
  }
}
