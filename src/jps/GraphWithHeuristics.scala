package jps

import scala.collection.immutable.Map
import scala.collection.immutable.List
import scala.collection.immutable.Vector


object GraphWithHeuristics {
  case class Node(key:Int)
  val nodes: Vector[Node] =
    Vector(
      Node(0),
      Node(1),
      Node(2),
      Node(3),
      Node(4)
  )

  val neighborsMap: Map[Node,List[(Node, Int)]] =
    Map(
      nodes(0) -> List((nodes(1),69),(nodes(3),135)),
      nodes(1) -> List((nodes(0),69), (nodes(2),75)),
      nodes(2) -> List((nodes(1),75), (nodes(3),165)),
      nodes(3) -> List((nodes(0),135), (nodes(2),165), (nodes(4),80)),
      nodes(4) -> List((nodes(2),180), (nodes(3),80))
    )

  val heuristics: List[Int] =
    List(
      200,
      247,
      162,
      72,
      0
    )

  val heuristicsMap: Map[Node,Int] = (nodes zip heuristics).toMap

  val firstNode: Node = nodes.head
  val lastNode: Node = nodes.last
  val startLimit: Option[Int] = heuristicsMap.get(firstNode)

  //returns path from start to the end and total cost
  def idaStar(): (List[Node],Int) = {
    deepen(startLimit)
  }

  def deepen(limit: Option[Int] ): (List[Node],Int) = {
    iteration(List(nodes(0)), 0, limit)
  }

  def iteration(nodes: List[Node], cost: Int, limit: Option[Int]): (List[Node],Int) = {
    val first = nodes.head
    first match {
      case `lastNode` => (List(first),cost)
      case _ => {
        val neighbors = neighborsMap.getOrElse(first,List()).unzip._1 //lista samych sasiadow
        val newNodes = neighbors :: nodes

        //temporary
        (List(first),cost)
    }
    }
  }
}
