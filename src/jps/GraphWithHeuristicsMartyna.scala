package jps

import jps.GraphWithHeuristicsMartyna.iteration

import scala.collection.immutable.{List, Map, SortedSet, Vector}


object GraphWithHeuristicsMartyna {

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
    val result = iteration(firstNode, List(), 0, limit)
    if (result._1.isEmpty)
      deepen(incrementLimit(startLimit))
    result
  }

  def incrementLimit( limit: Option[Int] ): Option[Int] ={
    limit match {
      case None => None
      case Some(_) => Option(limit.get + 1)
    }
  }
  def iteration(first: Option[Node], nodes: List[Node], cost: Int, limit: Option[Int]): (List[Node], Int) = {
    //val first: Option[Node] = nodes.headOption
    first match {
      case `lastNode` => (List(first.get), cost)
      //case _ if nodes.isEmpty => (List(), 0)
      case _ => {
        val neighbors: List[Node] = neighborsMap.getOrElse(first.get, Map()).keySet.toList //lista samych sasiadow
        println(nodes)
        println(neighbors)
        val newNodes: List[Node] = neighbors.filter(x => !nodes.exists(y => y == x)) ::: nodes
        println(newNodes)
        val head: Option[Node] = newNodes.headOption
        val rest = newNodes.drop(1)

        //val head2 = rest.headOption
        //val rest2 = rest.drop(1)
        //g(x) - total cost till head of nodes
        val updatedCost: Int = cost + neighborsMap.getOrElse(first.get, Map()).getOrElse(head.get,0)
        //val updatedCost2: Int = cost + neighborsMap.getOrElse(first.get, Map()).getOrElse(head2.get,0)
        println(updatedCost)

        //f(x) = g(x) + h(x)
        val fscore = updatedCost + heuristicsMap.getOrElse(head.get,0)

        if (fscore < limit.get)
          //val cos: List[Node] = iteration(head, rest, updatedCost, limit)._1.:+(first.get)
          (first.get +: iteration(head, rest, updatedCost, limit)._1, updatedCost)
        else {
          (first.get +: iteration(first, rest.drop(1), updatedCost, limit)._1, updatedCost)
        }


        //temporary
        //(List(first.get), cost)
      }
    }
  }

  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case i: List[_] => flatten(i)
    case e => List(e)
  }

  case class Node(key: Int)
}
