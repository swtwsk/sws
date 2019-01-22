package pl.edu.mimuw.sws

import scalaz._
import Scalaz._
import scalaz.zio.IO

import scala.annotation.tailrec

case class Arg(name: String)

case class UrlResolver(static: Option[String], collector: Option[StaticCollector]) {
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

    lazy val resolved = recResolve(path.split("/").toList.tailOption.getOrElse(Nil), pathTree, Map())

    static match {
      case Some(staticPath) =>
        val regex = (staticPath + """.*""").r
        regex.findFirstMatchIn(path) match {
          case Some(_) => collector match {
            case Some(c) => (c.serveStatic(_: Request)).right
            case None => Http404.left
          }
          case None => resolved
        }
      case None => resolved
    }
  }
}

object UrlResolver {
  type ArgsMap = Map[String, String]
  type Controller = (Request, ArgsMap) => Response
  type IOController = (Request, ArgsMap) => IO[Exception, Response]
  type RequestController = Request => IO[Exception, Response]

  def apply(static: Option[(String, String)]): UrlResolver = static match {
    case Some((staticPath, staticFolder)) => new UrlResolver(staticPath.some, StaticCollector(staticPath, staticFolder).some)
    case None => new UrlResolver(none, none)
  }
}
