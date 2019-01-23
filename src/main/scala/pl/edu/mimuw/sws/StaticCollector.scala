package pl.edu.mimuw.sws

import java.nio.file.{Files, Paths}

import scalaz._
import Scalaz._
import scalaz.zio.IO

case class StaticCollector(staticPaths: Option[(String, String)], favicon: Option[String]) {
  import scala.language.reflectiveCalls

  def serveStatic: Option[(String, Request => IO[Exception, Response])] = staticPaths map {
    case (staticPathPrefix: String, folderPath: String) => (
      staticPathPrefix,
      (request: Request) => serveStatic(folderPath + request.path.replaceFirst("^" + staticPathPrefix, ""))
    )
  }

  val serveFavicon: Option[Request => IO[Exception, Response]] = favicon map {
    s: String => (_: Request) => serveStatic(s)
  }

  private def serveStatic(filePath: String): IO[Exception, Response] = IO.syncException({
    val path = Paths.get(filePath)

    if (Files.exists(path) && !Files.isDirectory(path)) generateResponse(Files.readAllBytes(path), filePath)
    else HttpErrorResponse(Http404, content = Http404.toString)
  })

  private val extensionMap: Map[String, String] = Map(
    "txt" -> "text/plain",
    "html" -> "text/html",
    "js" -> "text/javascript",
    "css" -> "text/css",
    "xml" -> "text/xml",
    "zip" -> "application/zip",
    "png" -> "image/png",
    "jpg" -> "image/jpeg",
    "jpeg" -> "image/jpeg",
    "gif" -> "image/gif",
    "ico" -> "image/x-icon",
  )

  private def generateResponse(file: Array[Byte], relativePath: String): Response = {
    val fileExtensionDotPos = relativePath.lastIndexOf('.')
    val fileExtension = if (fileExtensionDotPos > 0) relativePath.substring(fileExtensionDotPos + 1).some else none
    val contentType = fileExtension match {
      case Some(fe) => extensionMap.getOrElse(fe, "")
      case None => ""
    }
    FileResponse(file.toList, contentType = contentType)
  }
}

object StaticCollector {
  def apply(paths: Option[(String, String)], favicon: Option[String]): StaticCollector =
    new StaticCollector(paths.map {
      case (staticPathPrefix: String, folderPath: String) =>
        (staticPathPrefix, folderPath + (if (folderPath.last != '/') "/" else ""))
    }, favicon)
}
