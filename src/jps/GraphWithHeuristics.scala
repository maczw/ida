package jps

import scala.collection.immutable.{List, Map, Vector}


object GraphWithHeuristics {

  val nodes: Vector[Node] =
    Vector(
      Node(0),
      Node(1),
      Node(2),
      Node(3),
      Node(4)
    )
  val neighborsMap: Map[Option[Node], List[(Option[Node], Int)] ] =
    Map(
      Some(nodes(0)) -> List((Some(nodes(1)), 69), (Some(nodes(3)), 135)),
      Some(nodes(1)) -> List((Some(nodes(0)), 69), (Some(nodes(2)), 75)),
      Some(nodes(2)) -> List((Some(nodes(1)), 75), (Some(nodes(3)), 165)),
      Some(nodes(3)) -> List((Some(nodes(0)), 135), (Some(nodes(2)), 165), (Some(nodes(4)), 80)),
      Some(nodes(4)) -> List((Some(nodes(2)), 180), (Some(nodes(3)), 80))
    )
  val heuristics: List[Int] =
    List(
      200,
      247,
      162,
      72,
      0
    )
  val heuristicsMap: Map[Option[Node], Int] = (nodes.map(x=>Option(x)) zip heuristics).toMap
  val firstNode: Option[Node] = nodes.headOption
  val lastNode: Option[Node] = nodes.lastOption
  val startLimit: Option[Int] = heuristicsMap.get(firstNode)

  //returns path from start to the end and total cost
  def idaStar(): (List[Node], Int) = {
    deepen(startLimit)
  }

  def deepen(limit: Option[Int]): (List[Node], Int) = {
    iteration(List(nodes(0)), 0, limit)
  }

  def iteration(nodes: List[Node], cost: Int, limit: Option[Int]): (List[Node], Int) = {
    val first: Option[Node] = nodes.headOption
    first match {
      case `lastNode` => (List(first.get), cost)
      case None => (List(), 0)
      case _ => {
        val neighbors = neighborsMap.getOrElse(first, List()).unzip._1 //lista samych sasiadow
        println(neighbors)
        //println(nodes.tail)
        val newNodes: List[Node] = flatten(neighbors :: nodes.tail)
        println(newNodes)
        val first2 = newNodes.headOption
        val newCost = cost + neighborsMap.get(first).get()


        //temporary
        (List(first.get), cost)
      }
    }
  }

  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case i: List[_] => flatten(i)
    case e => List(e)
  }

  case class Node(key: Int)
}
