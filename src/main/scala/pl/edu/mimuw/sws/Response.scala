package pl.edu.mimuw.sws

sealed trait Response {
  def content: String

  def contentType: String

  def statusCode: HttpStatus

  def charset: String

  def response: String = {
    "HTTP/1.1 " + statusCode + "\r\n" + (statusCode match {
      case Http200 =>
        "Content-Type: " + contentType + "; charset=" + charset + "\r\n" +
          "Content-Length: " + content.length + "\r\n" +
          "Connection: close\r\n" + "\r\n" + content
      case _ => "Connection: close\r\n" // temporary placeholder for other codes
    })
  }

  override def toString: String = response
}

case class HttpResponse(content: String,
                        contentType: String = "text/html",
                        statusCode: HttpStatus = Http200,
                        charset: String = "utf-8") extends Response
