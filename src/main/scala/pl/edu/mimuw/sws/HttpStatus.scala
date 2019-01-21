package pl.edu.mimuw.sws

sealed trait HttpStatus {
  def code: Int

  def reason: String

  def status: String = code.toString + " " + reason

  override def toString: String = status
}

sealed trait HttpRedirect extends HttpStatus
sealed trait HttpError extends HttpStatus

case class HttpUndefinedStatus(code: Int, reason: String) extends HttpStatus

case object Http200 extends HttpStatus {
  val code = 200
  val reason = "OK"
}

case object Http201 extends HttpStatus {
  val code = 201
  val reason = "Created"
}

case object Http204 extends HttpStatus {
  val code = 204
  val reason = "No Content"
}

case object Http302 extends HttpRedirect {
  val code = 302
  val reason = "Found"
}

case object Http400 extends HttpError {
  val code = 400
  val reason = "Bad Request"
}

case object Http401 extends HttpError {
  val code = 401
  val reason = "Unauthorized"
}

case object Http403 extends HttpError {
  val code = 403
  val reason = "Forbidden"
}

case object Http404 extends HttpError{
  val code = 404
  val reason = "Not Found"
}

case object Http405 extends HttpError {
  val code = 405
  val reason = "Method Not Allowed"
}

case object Http408 extends HttpError {
  val code = 408
  val reason = "Request Timeout"
}

case object Http414 extends HttpError {
  val code = 414
  val reason = "URI Too Long"
}

// We had to implement this
case object Http418 extends HttpError {
  val code = 418
  val reason = "I'm a teapot"
}

case object Http500 extends HttpError {
  val code = 500
  val reason = "Internal Server Error"
}

case object Http505 extends HttpError {
  val code = 505
  val reason = "HTTP Version Not Supported"
}