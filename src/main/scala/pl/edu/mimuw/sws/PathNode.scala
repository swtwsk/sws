package pl.edu.mimuw.sws

import pl.edu.mimuw.sws.UrlResolver.{Arg, Controller}

import scala.annotation.tailrec
import scalaz._
import Scalaz._

case class PathNode(children: List[PathNode], segment: Either[Arg, String], controller: Option[Controller])

object PathNode {
  def apply(urls: List[(String, Controller)]): PathNode = {
    val pathTree = urls.foldLeft(new PathNode(List(), Right(""), None)) {
      (acc: PathNode, el: (String, Controller)) =>
        val (path, view) = el
        addSon(acc, path.split("/").toList, view)
    }
    moveArgToBack(pathTree)
  }

  def apply(newChildren: List[PathNode], oldNode: PathNode): PathNode =
    new PathNode(newChildren, oldNode.segment, oldNode.controller)

  private val argRegexPattern = """\<(\w+)\>""".r

  private def addSon(node: PathNode, path: List[String], controller: Controller): PathNode = path match {
    case ph :: Nil => argRegexPattern.findFirstMatchIn(ph) match {
      case Some(arg) => new PathNode(List(), Left(Arg(arg.group(1))), controller.some)
      case None => new PathNode(List(), Right(ph), controller.some)
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