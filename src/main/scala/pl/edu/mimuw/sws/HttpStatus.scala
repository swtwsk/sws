package pl.edu.mimuw.sws

sealed trait HttpStatus {
  def code: Int

  def reason: String

  def status: String = code.toString + " " + reason

  override def toString: String = status
}

// TODO: Think about name convention
case class HttpUndefinedStatus(code: Int, reason: String) extends HttpStatus

case object Http200 extends HttpStatus {
  val code = 200
  val reason = "OK"
}

// TODO: HOW TO HANDLE REDIRECT ??? edit: HOW TO HANDLE ALL OF THE 3xx ???
case class Http302(url: String) extends HttpStatus {
  val code = 302
  val reason = "Found"
}

case object Http400 extends HttpStatus {
  val code = 400
  val reason = "Bad Request"
}

case object Http404 extends HttpStatus{
  val code = 404
  val reason = "Not Found"
}
