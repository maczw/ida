package jps

import scala.collection.immutable.{List, Map, SortedSet, Vector}

object GraphWithHeuristics {
  case class Node(key: Int)
  val nodes: Vector[Node] =
    Vector(
      Node(0),
      Node(1),
      Node(2),
      Node(3),
      Node(4)
    )

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

//  val nodes: Vector[Node] =
//    Vector(
//      Node(0),
//      Node(1),
//      Node(2),
//      Node(3),
//      Node(4),
//      Node(5),
//      Node(6),
//      Node(7),
//    )
//
//  val neighborsMap: Map[Node, Map[Node, Int] ] =
//    Map(
//      nodes(0) -> Map(nodes(1) -> 6, nodes(2) -> 5, nodes(3) -> 10),
//      nodes(1) -> Map(nodes(0)-> 6, nodes(4)-> 6),
//      nodes(2) -> Map(nodes(0)-> 5, nodes(4)-> 6, nodes(5)-> 7),
//      nodes(3) -> Map(nodes(0)-> 10, nodes(5)-> 6),
//      nodes(4) -> Map(nodes(1)-> 6, nodes(2)-> 6, nodes(6)-> 4),
//      nodes(5) -> Map(nodes(2)-> 7, nodes(3)-> 6, nodes(6)-> 6),
//      nodes(6) -> Map(nodes(4)-> 4, nodes(5)-> 6, nodes(7)-> 3),
//      nodes(7) -> Map(nodes(6)-> 3)
//    )
//
//  val heuristics: List[Int] =
//    List(
//      12,
//      10,
//      13,
//      4,
//      4,
//      2,
//      1,
//      0
//    )
//
//  val heuristicsMap: Map[Node, Int] = (nodes zip heuristics).toMap
//  val firstNode: Option[Node] = nodes.headOption
//  val lastNode: Option[Node] = nodes.lastOption
//  val startLimit: Option[Int] = heuristicsMap.get(firstNode.get)

  //returns path from start to the end and total cost
  def idaStar(): (List[Node], Int) = {
    if (firstNode.isEmpty)
      throw new Exception("The graph is empty")
    val result = deepen(startLimit)
    (result._1, result._2)
  }

  def deepen(limit: Option[Int]): (List[Node], Int, SortedSet[Int]) = {
    val result = iteration(firstNode, List(), 0, limit,true)
    if (result._1.isEmpty)
      deepen(incrementLimit(result._3))
    else
      result
  }

  def incrementLimit(availableLimits: SortedSet[Int] ): Option[Int] ={
    println(availableLimits)
    Option(availableLimits.min)
  }

  def iteration(first: Option[Node], nodes: List[Node], cost: Int, limit: Option[Int], flag: Boolean): (List[Node], Int, SortedSet[Int]) = {
    //val first: Option[Node] = nodes.headOption
    first match {
      case `lastNode` => (List(first.get), cost, SortedSet())
      case _ if nodes.isEmpty && !flag => (List(), 0, SortedSet())
      case _ => {
        val neighbors: List[Node] = neighborsMap.getOrElse(first.get, Map()).keySet.toList
        println(nodes)
        println(neighbors)

        val newNodes: List[Node] = {
          if (flag) neighbors.filter(x => !nodes.exists(y => y == x)) ::: nodes
          else nodes
        }
        println(newNodes)

        val head: Option[Node] = newNodes.headOption
        val rest = newNodes.drop(1)

        //g(x) - total cost till head to this
        val updatedCost: Int = cost + neighborsMap.getOrElse(first.get, Map()).getOrElse(head.get,0)
        println(updatedCost)

        //f(x) = g(x) + h(x)
        val fscore = updatedCost + heuristicsMap.getOrElse(head.get,0)
        val iterationResult: (List[Node], Int, SortedSet[Int]) = {
          if (fscore <= limit.get)
            iteration(head, rest, updatedCost, limit,true)
          else
            iteration(first, rest, cost, limit,false)
        }

        val result: (List[Node], Int, SortedSet[Int]) = {
          if (iterationResult._1.contains(first.get) || iterationResult._1.isEmpty)
            (iterationResult._1,
              iterationResult._2,
              if (fscore > limit.get) iterationResult._3 + fscore else iterationResult._3)
          else
            (first.get +: iterationResult._1,
              iterationResult._2,
              if (fscore > limit.get) iterationResult._3 + fscore else iterationResult._3)
        }
        result
      }
    }
  }
}
