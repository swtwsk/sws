package pl.edu.mimuw.sws

import scalaz._
import Scalaz._
import scalaz.zio.IO

import scala.annotation.tailrec

case class Arg(name: String)

case class UrlResolver(collector: StaticCollector) {
  import UrlResolver.{ArgsMap, RequestController}

  def resolve(path: String, pathTree: PathNode): \/[HttpError, RequestController] = {
    @tailrec
    def recResolve(pl: List[String], t: PathNode, args: ArgsMap): \/[HttpError, RequestController] =
      pl match {
        case p :: pt => t.children find {
          _.segment match {
            case Left(_) => true
            case Right(s) => s == p
          }
        } match {
          case Some(c) => c.segment match {
            case Left(Arg(name)) => recResolve(pt, c, args + (name -> p))
            case Right(_) => recResolve(pt, c, args)
          }
          case None => Http404.left
        }
        case Nil => t.controller match {
          case Some(controller) => (controller(_: Request, args)).right
          case None => Http404.left
        }
      }

    val serveStatic = collector.serveStatic flatMap {
      case (staticPathPrefix, serveFunction) =>
        val regex = ("""^""" + staticPathPrefix + """.*""").r
        regex.findFirstMatchIn(path) match {
          case Some(_) => serveFunction.some
          case None => none
        }
    }
    val serveFavicon = collector.serveFavicon flatMap {
      serveFunction =>
        val regex = """^\/favicon\.ico$""".r
        regex.findFirstMatchIn(path) match {
          case Some(_) => serveFunction.some
          case None => none
        }
    }

    serveStatic match {
      case Some(serveFunction) => serveFunction.right
      case None => serveFavicon match {
        case Some(serveFunction) => serveFunction.right
        case None => recResolve(path.split("/").toList.tailOption.getOrElse(Nil), pathTree, Map())
      }
    }
  }
}

object UrlResolver {
  type ArgsMap = Map[String, String]
  type Controller = (Request, ArgsMap) => Response
  type IOController = (Request, ArgsMap) => IO[Exception, Response]
  type RequestController = Request => IO[Exception, Response]

  def apply(static: Option[(String, String)], favicon: Option[String]): UrlResolver =
    new UrlResolver(StaticCollector(static, favicon))
}
