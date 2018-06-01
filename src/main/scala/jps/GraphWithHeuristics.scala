package scala.jps

import scala.annotation.tailrec
import scala.collection.immutable.{List, Map, SortedSet, Vector}
import scala.io.Source

class GraphWithHeuristics(val path: String) {
  case class Node(key: Int)

//  val nodes: Vector[Node] =
//    Vector(
//      Node(0),
//      Node(1),
//      Node(2),
//      Node(3),
//      Node(4)
//    )

//  val neighborsMap: Map[Node, Map[Node, Int] ] =
//    Map(
//      nodes(0) -> Map(nodes(1) -> 69, nodes(3) -> 135),
//      nodes(1) -> Map(nodes(0)-> 69, nodes(2)-> 75),
//      nodes(2) -> Map(nodes(1)-> 75, nodes(3)-> 165),
//      nodes(3) -> Map(nodes(0)-> 135, nodes(2)-> 165, nodes(4)-> 80),
//      nodes(4) -> Map(nodes(2)-> 180, nodes(3)-> 80)
//    )

//  val heuristics: List[Int] =
//    List(
//      200,
//      247,
//      162,
//      72,
//      0
//    )

  val nodes: Vector[Node] = Source.fromFile(path + "//" + "nodes").getLines.map(x=>Node(x.toInt)).toVector

  val neighborsMap: Map[Node, Map[Node, Int] ] = {
    Source.fromFile(path + "//" + "neighbors").getLines
      .map(m =>
        (
          Node( m.split("->")(0).toInt ),
          m.split("->")(1).split(",").toList
            .map(
              m1 =>
                (Node(m1.split("-")(0).toInt),
                  m1.split("-")(1).toInt
                )
            ).toMap
        )
      )
      .toMap
  }

  val heuristics: List[Int] = Source.fromFile(path + "//" + "heuristics").getLines.map(_.toInt).toList

//  println(nodes)
//  println(neighborsMap)
//  println(heuristics)

  val heuristicsMap: Map[Node, Int] = (nodes zip heuristics).toMap
  val firstNode: Option[Node] = nodes.headOption
  val lastNode: Option[Node] = nodes.lastOption
  val startLimit: Option[Int] = heuristicsMap.get(firstNode.get)

  //returns path from start to the end and total cost
  def idaStar(): (List[Node], Int) = {
    if (firstNode.isEmpty)
      throw new Exception("The graph is empty")
    val result = deepen(startLimit)
    (result._1, result._2)
  }

  @tailrec
  private def deepen(limit: Option[Int]): (List[Node], Int, SortedSet[Int]) = {
    val result = iteration(firstNode, List(), 0, limit,true)
    if (result._1.isEmpty)
      deepen(incrementLimit(result._3))
    else
      result
  }

  def incrementLimit(availableLimits: SortedSet[Int] ): Option[Int] = Option(availableLimits.min)

  def iteration(first: Option[Node], nodes: List[Node], cost: Int, limit: Option[Int], flag: Boolean): (List[Node], Int, SortedSet[Int]) = {
    first match {
      case `lastNode` => (List(first.get), cost, SortedSet())
      case _ if nodes.isEmpty && !flag => (List(), 0, SortedSet())
      case _ =>
        val neighbors: List[Node] = neighborsMap.getOrElse(first.get, Map()).keySet.toList

        val newNodes: List[Node] = {
          if (flag) neighbors.filter(x => !nodes.exists(y => y == x)) ::: nodes
          else nodes
        }

        val head: Option[Node] = newNodes.headOption
        val rest = newNodes.drop(1)

        //g(x) - total cost till head to this
        val updatedCost: Int = cost + neighborsMap.getOrElse(first.get, Map()).getOrElse(head.get,0)

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