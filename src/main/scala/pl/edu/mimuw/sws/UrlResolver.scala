package pl.edu.mimuw.sws

import scalaz._
import Scalaz._

import scala.annotation.tailrec

object UrlResolver {
  case class Arg(name: String)
  type Controller = (Request, Map[String, String]) => Response
  type RequestController = Request => Response

  def resolve(path: String, pathTree: PathNode): \/[HttpError, RequestController] = {
    @tailrec
    def recResolve(pl: List[String], t: PathNode, args: Map[String, String]): \/[HttpError, RequestController] =
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
          case Some(view) => (view(_: Request, args)).right
          case None => Http404.left
        }
      }

    recResolve(path.split("/").toList.tailOption.getOrElse(Nil), pathTree, Map())
  }
}
