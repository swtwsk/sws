package pl.edu.mimuw.sws

sealed trait Response {
  def statusCode: HttpStatus

  def headers: Map[String, String]

  def response: List[Byte] = {
    ("HTTP/1.1 " + statusCode + "\r\n" +
      (getHeaders foldLeft "") {_ + _ + "\r\n"} + "\r\n").toList.map(_.toByte)
  }

  protected def getHeaders: List[String] =
    (headers map {h => h._1 + ": " + h._2}).toList
}

sealed trait BodyResponse extends Response {
  def byteContent: List[Byte]

  def contentType: String

  def charset: String

  private def isText: Boolean =
    """text\/\w*""".r
      .findFirstMatchIn(contentType).isDefined

  private def contentTypeHeader: String =
    "Content-Type: " + contentType + (if (isText && !charset.isEmpty) "; charset=" + charset else "")

  override protected def getHeaders: List[String] = {
    val headersList = ((headers + ("Content-Length" -> byteContent.length.toString)) map {
      h => h._1 + ": " + h._2
    }).toList

    if (contentType.isEmpty) headersList else contentTypeHeader :: headersList
  }

  override def response: List[Byte] = super.response ::: byteContent
}

sealed trait StringResponse extends BodyResponse {
  def content: String

  override def byteContent: List[Byte] = content.toList.map(_.toByte)
}

case class HttpResponse(content: String,
                        contentType: String = "text/html",
                        statusCode: HttpStatus = Http200,
                        charset: String = "utf-8",
                        headers: Map[String, String] = Map(),
                        cookies: List[Cookie] = List()) extends StringResponse {
  override protected def getHeaders: List[String] =
    super.getHeaders ::: (cookies map {"Set-Cookie: " + _})
}

case class HttpRedirectResponse(location: String,
                                statusCode: HttpRedirect = Http302) extends Response {
  override def headers: Map[String, String] = Map("Location" -> location)
}

case class HttpErrorResponse(statusCode: HttpError,
                             content: String = "",
                             contentType: String = "text/html",
                             charset: String = "utf-8",
                             headers: Map[String, String] = Map()) extends StringResponse

case class FileResponse(byteContent: List[Byte],
                        contentType: String = "",
                        statusCode: HttpStatus = Http200,
                        charset: String = "",
                        headers: Map[String, String] = Map()) extends BodyResponse