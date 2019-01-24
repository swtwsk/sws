package pl.edu.mimuw.sws

import pl.edu.mimuw.sws.UrlResolver.{ArgsMap, Controller, IOController}

import scala.annotation.tailrec
import scalaz._
import Scalaz._
import scalaz.zio.IO

case class PathNode(children: List[PathNode], segment: Either[Arg, String], controller: Option[IOController])

object PathNode {
  def apply(urls: List[(String, Controller)], urlsIO: List[(String, IOController)]): PathNode = {
    def liftToIO(controller: Controller): IOController =
      (r: Request, args: ArgsMap) => IO.syncException(controller(r, args))

    def urlFoldFun(acc: PathNode, el: (String, IOController)): PathNode = addSon(acc, el._1.split("/").toList, el._2)

    val pathTree = urls.foldLeft(new PathNode(List(), Right(""), None)) {
      (acc, el) => urlFoldFun(acc, el match {case (s, c) => (s, liftToIO(c))})
    }
    val pathTreeWithIO = urlsIO.foldLeft(pathTree)(urlFoldFun)
    moveArgToBack(pathTreeWithIO)
  }

  def apply(newChildren: List[PathNode], oldNode: PathNode): PathNode =
    new PathNode(newChildren, oldNode.segment, oldNode.controller)

  private val argRegexPattern = """\<(\w+)\>""".r

  private def addSon(node: PathNode, path: List[String], controller: IOController): PathNode = path match {
    case ph :: Nil => argRegexPattern.findFirstMatchIn(ph) match {
      case Some(arg) => new PathNode(node.children, Left(Arg(arg.group(1))), controller.some)
      case None => new PathNode(node.children, Right(ph), controller.some)
    }
    case _ :: ph :: pt => node.children find {
      _.segment match {
        case Right(s) => s == ph
        case Left(_) => argRegexPattern.findFirstMatchIn(ph).isDefined
      }
    } match {
      case Some(son) => PathNode(
        node.children.map {
          case child if child == son => addSon(son, ph :: pt, controller)
          case child => child
        },
        node
      )
      case None => PathNode(
        addSon(new PathNode(List(), Right(ph), None), ph :: pt, controller) :: node.children,
        node
      )
    }
    case Nil => throw new Exception("Your `urls` list is in invalid format") // TODO: More explicit (type) exception
  }

  private def moveArgToBack(node: PathNode): PathNode = {
    @tailrec
    def move(front: List[PathNode], back: List[PathNode]): List[PathNode] = back match {
      case h :: t => h.segment match {
        case Left(_) => front.reverse ::: t ::: List(h)
        case Right(_) => move(h :: front, t)
      }
      case Nil => front.reverse
    }

    PathNode(move(List(), node.children.map(moveArgToBack)), node)
  }
}