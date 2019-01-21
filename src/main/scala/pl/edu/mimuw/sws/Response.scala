package pl.edu.mimuw.sws

sealed trait Response {
  def statusCode: HttpStatus

  def headers: Map[String, String]

  def response: String = {
    "HTTP/1.1 " + statusCode + "\r\n" +
      (getHeaders foldLeft "") {_ + _ + "\r\n"} + "\r\n"
  }

  override def toString: String = response

  protected def getHeaders: List[String] =
    (headers map {h => h._1 + ": " + h._2}).toList
}

sealed trait BodyResponse extends Response {
  def content: String

  def contentType: String

  def charset: String

  private def isText: Boolean =
    """text\/\w*""".r
      .findFirstMatchIn(contentType).isDefined

  override protected def getHeaders: List[String] =
    ("Content-Type: " + contentType + (if (isText) "; charset=" + charset)) ::
      ((headers + ("Content-Length" -> content.length.toString)) map { h => h._1 + ": " + h._2 }).toList


  override def response: String = super.response + content
}

case class HttpResponse(content: String,
                        contentType: String = "text/html",
                        statusCode: HttpStatus = Http200,
                        charset: String = "utf-8",
                        headers: Map[String, String] = Map(),
                        cookies: Map[String, String] = Map()) extends BodyResponse {
  override protected def getHeaders: List[String] =
    super.getHeaders ::: (cookies map {e => "Set-Cookie: " + e._1 + "=" + e._2}).toList
}

// TODO: JsonResponse

case class HttpRedirectResponse(location: String,
                                statusCode: HttpRedirect = Http302) extends Response {
  override def headers: Map[String, String] = Map("Location" -> location)
}

case class HttpErrorResponse(statusCode: HttpError,
                             content: String = "",
                             contentType: String = "text/html",
                             charset: String = "utf-8",
                             headers: Map[String, String] = Map()) extends BodyResponse