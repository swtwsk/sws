package pl.edu.mimuw.testapi

import java.nio.file.{Files, Paths}

import pl.edu.mimuw.sws.UrlResolver.IOController
import scalaz.Scalaz._
import scalaz.zio.IO
import pl.edu.mimuw.sws._

case object TestMain extends ServerMain {
  override val defaultConfigFile: String = "default.conf"
  val urls = List(
    ("/test1", cssView),
    ("/test4", getView),
    ("/test5", abortView),
    ("/test6", mp3View),
    ("/redirect", googleRedirect),
    ("/counter", cookieView),
    ("/secret/<name>", argView),
  )

  override val urlsIO: List[(String, IOController)] = List(("", indexView))

  override val static: Option[(String, String)] = Some("/static", System.getProperty("user.dir") + "/static")
  override val favicon: Option[String] = Some(System.getProperty("user.dir") + "/favicon.ico")

  def indexView(request: Request, args: Map[String, String]): IO[Exception, Response] = IO.syncException({
    HttpResponse(new String(Files.readAllBytes(Paths.get(System.getProperty("user.dir") + "/templates/index.html"))))
  })

  def argView(request: Request, args: Map[String, String]): Response = HttpResponse(
    """<!DOCTYPE html>
      | <html lang="en">
      | <head>
      |     <title>Scala Server >> Secret page</title>
      | </head>
      | <body>
      |     <div id="content">
      |         <h2>Wow! """.stripMargin +
      args.getOrElse("name", "").toLowerCase.capitalize +
      """, you have found the secret page!</h2>
      |         <p><a href="/">go back</a></p>
      |     </div>
      |     <div id="footer">
      |         Copyright 2019 Andrzej Swatowski, Frederic Grabowski.
      |     </div>
      | </body>
      | </html>""".stripMargin
  )

  def mp3View(request: Request, args: Map[String, String]): Response = HttpResponse(
    """<!DOCTYPE html>
      | <html lang="en">
      | <head>
      |     <title>Scala Server >> MP3</title>
      | </head>
      | <body>
      |     <div id="content">
      |         <audio src="/static/crabrave.mp3" autoplay>
      |             <p>If you are reading this, it is because your browser does not support the audio element.</p>
      |         </audio>
      |         <p><a href="/">go back</a></p>
      |     </div>
      |     <div id="footer">
      |         Copyright 2019 Andrzej Swatowski, Frederic Grabowski.
      |     </div>
      | </body>
      | </html>""".stripMargin
  )

  def cssView(request: Request, args: Map[String, String]): Response = HttpResponse(
    """<!DOCTYPE html>
      | <html lang="en">
      | <head>
      |     <link rel="stylesheet" href="/static/style.css">
      |     <title>Scala Server >> Test 1</title>
      | </head>
      | <body>
      |     <div id="content">
      |         <h2>Testing CSS!</h2>
      |         <p><a href="/">go back</a></p>
      |     </div>
      |     <div id="footer">
      |         Copyright 2019 Andrzej Swatowski, Frederic Grabowski.
      |     </div>
      | </body>
      | </html>""".stripMargin
  )

  def getView(request: Request, args: Map[String, String]): Response = HttpResponse(
    """<!DOCTYPE html>
      | <html lang="en">
      | <head>
      |     <title>Scala Server >> GET</title>
      | </head>
      | <body>
      |     <div id="content">
      |         <h2>Hello,""".stripMargin + request.query.getOrElse("name", "name") + """!</h2>
      |         <p><a href="/">go back</a></p>
      |
      |     </div>
      |     <div id="footer">
      |         Copyright 2019 Andrzej Swatowski, Frederic Grabowski.
      |     </div>
      | </body>
      | </html>""".stripMargin
  )

  def abortView(request: Request, args: Map[String, String]): Response = HttpErrorResponse(Http404, Http404.toString)

  def testView(request: Request, args: Map[String, String]): Response = HttpResponse(request.responseBody)

  def googleRedirect(request: Request, args: Map[String, String]): Response = HttpRedirectResponse("http://google.com")

  def cookieView(request: Request, args: Map[String, String]): Response = {
    val counter = request.cookies.getOrElse("counter", "0").parseInt.toOption.getOrElse(0) + 1

    HttpResponse(
      """<!DOCTYPE html>
        | <html lang="en">
        | <head>
        |     <title>Scala Server >> Counter</title>
        | </head>
        | <body>
        |     <div id="content">
        |         <h2>Counter</h2>
        |         <p>You've visited us <b>""".stripMargin + counter + """</b> times!</p>
        |         <p><a href="/">go back</a></p>
        |     </div>
        |     <div id="footer">
        |         Copyright 2019 Andrzej Swatowski, Frederic Grabowski.
        |     </div>
        | </body>
        | </html>""".stripMargin,
      cookies = Cookie("counter", counter.toString) :: Nil
    )
  }
}