package pl.edu.mimuw.sws

import scalaz._
import Scalaz._

import scala.annotation.tailrec

object UrlResolver {
  case class Arg(name: String)
  private type Val = Either[Arg, String]
  type View = (Request, Map[String, String]) => Response
  type ReqView = Request => Response

  case class PathNode(children: List[PathNode], x: Val, v: Option[View])
  object PathNode {
    def apply(urls: List[(String, View)]): PathNode = {
      val pathTree = urls.foldLeft(new PathNode(List(), Right(""), None)) {(acc: PathNode, el: (String, View)) =>
        val (path, view) = el
        addSon(acc, path.split("/").toList, view)
      }
      moveArgToBack(pathTree)
    }

    private val argRegexPattern = """\<(\w+)\>""".r

    private def addSon(node: PathNode, path: List[String], view: View): PathNode = path match {
      case ph :: Nil => argRegexPattern.findFirstMatchIn(ph) match {
        case Some(arg) => new PathNode(List(), Left(Arg(arg.group(1))), view.some)
        case None => new PathNode(List(), Right(ph), view.some)
      }
      case _ :: ph :: pt => node.children find {
        _.x match {
          case Right(s) => s == ph
          case Left(_) => argRegexPattern.findFirstMatchIn(ph).isDefined
        }
      } match {
        case Some(son) => new PathNode(node.children.map {case x if x == son => addSon(son, ph :: pt, view); case x => x}, node.x, node.v)
        case None => new PathNode(addSon(new PathNode(List(), Right(ph), None), ph :: pt, view) :: node.children, node.x, node.v)
      }
      case Nil => throw new Exception("IT NEVER HAPPENS")  // TODO: More explicit exception
    }

    private def moveArgToBack(node: PathNode): PathNode = {
      @tailrec
      def move(front: List[PathNode], back: List[PathNode]): List[PathNode] = back match {
        case h :: t => h.x match {
          case Left(_) => front.reverse ::: t ::: List(h)
          case Right(_) => move(h :: front, t)
        }
        case Nil => front.reverse
      }

      PathNode(move(List(), node.children.map(moveArgToBack)), node.x, node.v)
    }
  }

//  def indexView(request: Request, args: Map[String, String]): Response = HttpResponse(
//    "<html><head><title>Index</title></head><body>" +
//      "<div><b>Index</b></div>" +
//      "</body></html>"
//  )
//
//  def aView(request: Request, args: Map[String, String]): Response = HttpResponse(
//    "<html><head><title>a</title></head><body>" +
//      "<div><b>a</b></div>" +
//      "</body></html>"
//  )
//
//  def bView(request: Request, args: Map[String, String]): Response = HttpResponse(
//    "<html><head><title>b</title></head><body>" +
//      "<div><b>b</b></div>" +
//      "</body></html>"
//  )
//
//  def aaView(request: Request, args: Map[String, String]): Response = HttpResponse(
//    "<html><head><title>a</title></head><body>" +
//      "<div><b>a/cos</b></div>" +
//      "<div><b>Args: </b>" + args.mkString + "</div>" +
//      "</body></html>"
//  )

//  val tree: PathNode = PathNode(List(
//    PathNode(List(PathNode(List(), Left(Arg("cos")), aaView)), Right("a"), aView),
//    PathNode(List(), Right("b"), bView)
//  ), Right(""), indexView)
  val tree: PathNode = PathNode(List())

  def resolve(path: String, pathTree: PathNode): \/[HttpError, ReqView] = {
    @tailrec
    def recResolve(pl: List[String], t: PathNode, args: Map[String, String]): \/[HttpError, ReqView] = pl match {
      case p :: pt => t.children find {
        _.x match {
          case Left(_) => true
          case Right(s) => s == p
        }
      } match {
        case Some(c) => c.x match {
          case Left(Arg(name)) => recResolve(pt, c, args + (name -> p))
          case Right(_) => recResolve(pt, c, args)
        }
        case None => Http404.left
      }
      case Nil => t.v match {
        case Some(view) => (view(_: Request, args)).right
        case None => Http404.left
      }
    }

    recResolve(path.split("/").toList.tailOption.getOrElse(Nil), pathTree, Map())
  }
}
