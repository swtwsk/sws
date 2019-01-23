package pl.edu.mimuw.testapi

import java.util.{Calendar, Date}

import pl.edu.mimuw.sws._

case object TestMain extends ServerMain {
  val urls = List(
    ("", indexView),
    ("/a", pathView("/a", _: Request, _: Map[String, String])),
    ("/b", pathView("/b", _: Request, _: Map[String, String])),
    ("/<id>", pathView("/<id>", _: Request, _: Map[String, String])),
    ("/a/a", pathView("/a/a", _: Request, _: Map[String, String])),
    ("/a/b", pathView("/a/b", _: Request, _: Map[String, String])),
    ("/a/c", pathView("/a/c", _: Request, _: Map[String, String])),
    ("/a/<id>", pathView("/a/<id>", _: Request, _: Map[String, String])),
    ("/a/b/<name>", pathView("/a/b/<name>", _: Request, _: Map[String, String])),
    ("/a/b/<name>/id/<id>", pathView("/a/b/<name>/id/<id>", _: Request, _: Map[String, String])),
    ("/test", testView),
    ("/redirect", googleRedirect),
    ("/cookie", setCookieView),
  )

  def indexView(request: Request, args: Map[String, String]): Response = HttpResponse(
    "<html><head><title>Index</title></head><body>" +
      "<div><b>Index</b></div>" +
      "</body></html>"
  )

  def pathView(path: String, request: Request, args: Map[String, String]): Response = HttpResponse(
    "<html><head><title>" + path + "</title></head><body>" +
      "<div text-align=\"center\"><b>" + path + "</b></div>" +
      "<div><b>Args: </b>" + args.mkString + "</div>" +
      "</body></html>"
  )

  def testView(request: Request, args:Map[String, String]): Response = HttpResponse(request.responseBody)

  def googleRedirect(request: Request, args: Map[String, String]): Response = HttpRedirectResponse("http://google.com")

  def setCookieView(request: Request, args:Map[String, String]): Response = HttpResponse(
    "<html><head><title>Set Cookie</title></head><body>" +
      "<div><b>theme</b> = <b>light</b></div>" +
      "</body></html>",
    cookies = Cookie(request.query.getOrElse("key", "theme"), request.query.getOrElse("value", "light")) ::
      Cookie("klucz", "wartosc", Some(Calendar.getInstance.getTime), maxAge = None, path = Some("/")) ::
      Nil
  )
}