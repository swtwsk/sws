package pl.edu.mimuw.sws

import java.nio.file.{Files, Paths}

import scalaz._
import Scalaz._
import scalaz.zio.IO

case class StaticCollector(staticPathPrefix: String, folderPath: String) {
  import scala.language.reflectiveCalls

  def serveStatic(request: Request): IO[Exception, Response] = serveStatic(request.path)

  private def serveStatic(relativePath: String): IO[Exception, Response] = IO.syncException({
    val trimmedRelativePath = relativePath.replaceFirst("^" + staticPathPrefix, "")
    val path = Paths.get(folderPath + trimmedRelativePath)

    if (Files.exists(path) && !Files.isDirectory(path)) generateResponse(Files.readAllBytes(path), trimmedRelativePath)
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
  )

  private val unknownExtension: String = "application/octet-stream"

  private def generateResponse(file: Array[Byte], relativePath: String): Response = {
    val fileExtensionDotPos = relativePath.lastIndexOf('.')
    val fileExtension = if (fileExtensionDotPos > 0) relativePath.substring(fileExtensionDotPos + 1).some else none
    val contentType = fileExtension match {
      case Some(fe) => extensionMap.getOrElse(fe, unknownExtension)
      case None => unknownExtension
    }
    FileResponse(file.toList, contentType = contentType)
  }
}

object StaticCollector {
  def apply(staticPathPrefix: String, folderPath: String): StaticCollector =
    new StaticCollector(staticPathPrefix: String, folderPath + (if (folderPath.last != '/') "/" else ""))
}
