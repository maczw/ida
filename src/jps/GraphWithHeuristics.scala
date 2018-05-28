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
//  val neighborsMap: Map[Node, List[(Node, Int)] ] =
//    Map(
//      nodes(0) -> List((nodes(1), 69), (nodes(3), 135)),
//      nodes(1) -> List((nodes(0), 69), (nodes(2), 75)),
//      nodes(2) -> List((nodes(1), 75), (nodes(3), 165)),
//      nodes(3) -> List((nodes(0), 135), (nodes(2), 165), (nodes(4), 80)),
//      nodes(4) -> List((nodes(2), 180), (nodes(3), 80))
//    )

  val neighborsMap: Map[Node, Map[Node, Int] ] =
    Map(
      nodes(0) -> Map(nodes(1) -> 69, nodes(3) -> 135),
      nodes(1) -> Map(nodes(0)-> 69, nodes(2)-> 75),
      nodes(2) -> Map(nodes(1)-> 75, nodes(3)-> 165),
      nodes(3) -> Map(nodes(0)-> 135, nodes(2)-> 165, nodes(4)-> 80),
      nodes(4) -> Map(nodes(2)-> 180, nodes(3)-> 80)
    )
  val heuristics: List[Int] =
    List(
      200,
      247,
      162,
      72,
      0
    )
  val heuristicsMap: Map[Node, Int] = (nodes zip heuristics).toMap
  val firstNode: Option[Node] = nodes.headOption
  val lastNode: Option[Node] = nodes.lastOption
  val startLimit: Option[Int] = heuristicsMap.get(firstNode.get)

  //returns path from start to the end and total cost
  def idaStar(): (List[Node], Int) = {
    if (firstNode.isEmpty)
      throw new Exception("The graph is empty")
    deepen(startLimit)
  }

  def deepen(limit: Option[Int]): (List[Node], Int) = {
    iteration(firstNode, List(), 0, limit)
    //iteration(None, List(), 0, limit)
  }

  def iteration(first: Option[Node], nodes: List[Node], cost: Int, limit: Option[Int]): (List[Node], Int) = {
    //val first: Option[Node] = nodes.headOption
    first match {
      case `lastNode` => (List(first.get), cost)
      //case None => (List(), 0)
      case _ => {

          val neighbors: List[Node] = neighborsMap.getOrElse(first.get, Map()).keySet.toList //lista samych sasiadow
          //println(neighbors)
          //println(nodes.tail)
          val newNodes: List[Node] = neighbors.filter(x => !nodes.exists(y => y == x)) ::: nodes
          //println(newNodes)
          val head: Option[Node] = newNodes.headOption

          val updatedCost: Int = cost + neighborsMap.getOrElse(first.get, Map()).getOrElse(head.get,0)
          println(updatedCost)




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
