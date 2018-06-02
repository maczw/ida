package scala.jps

import scala.annotation.tailrec
import scala.collection.immutable.{List, Map, SortedSet, Vector}
import scala.io.Source
/**
  * A class to represent the graph and implement IDA* algoritm [[https://en.wikipedia.org/wiki/Iterative_deepening_A*]]
  *
  * Specify `path` to your resources when creating a new `GraphWithHeuristics`
  * your resources should contain:
  *
  * @author Martyna Czwarno, Milena Jachimowska
  * @version 1.0
  * @see [[https://github.com/maczw/ida]] - our repository on github
  *
  * @example
  *
  * run scala.jps.Main "C:\ida\src\main\resources\graph1"
  *
  *
  * ==Your resources should contain three files defining graph:==
  * nodes: {{{
  *   1
  *   2
  *   3
  *   4
  *   5
  * }}}
  *
  * heuristics: {{{
  *   200
  *   247
  *   162
  *   72
  *   0
  * }}}
  *
  * neighbors: {{{
  *   0->1-69,3-135
  *   1->0-69,2-75
  *   2->1-75,3-165
  *   3->0-135,2-165,4-80
  *   4->2-180,3-80
  * }}}
  *
  *
  *
  */
class GraphWithHeuristics(val path: String) {

  /**
    * A class to represent a single node in the graph
    * @param key label of the node
    */
  case class Node(key: Int)

  /**
    * vector of nodes read from file
    */
  val nodes: Vector[Node] = Source.fromFile(path + "//" + "nodes").getLines.map(x=>Node(x.toInt)).toVector

  /**
    * map connecting nodes to their neighbors
    */
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

  /**
    * list of heuristic function values read from file
    */
  val heuristics: List[Int] = Source.fromFile(path + "//" + "heuristics").getLines.map(_.toInt).toList

  //  println(nodes)
  //  println(neighborsMap)
  //  println(heuristics)
  /**
    * map connecting nodes with their values of heuristic function
    */
  val heuristicsMap: Map[Node, Int] = (nodes zip heuristics).toMap
  /**
    *  the first node of the path
    */
  val firstNode: Option[Node] = nodes.headOption
  /**
    *  the final node of the path
    */
  val lastNode: Option[Node] = nodes.lastOption
  /**
    * limit for the first recursion of IDA* algorithm is the value of heuristic function of the starting node
    */
  val startLimit: Option[Int] = heuristicsMap.get(firstNode.get)

  /**
    * implementation of IDA* algorithm
    * @return path from the first to the last node and total cost
    * @throws Exception if given graph is empty
    */
  def idaStar(): (List[Node], Int) = {
    if (firstNode.isEmpty)
      throw new Exception("The graph is empty")
    val result = deepen(startLimit)
    (result._1, result._2)
  }

  /**
    * one recursion of the algorithm
    * @param limit
    * @return current path, limit,
    */
  @tailrec
  private def deepen(limit: Option[Int]): (List[Node], Int, SortedSet[Int]) = {
    val result = iteration(firstNode, List(), 0, limit,true)
    if (result._1.isEmpty)
      deepen(incrementLimit(result._3))
    else
      result
  }

  /**
    * increments limit for IDA* algorithm
    * @param availableLimits
    * @return incremented limit - the next minimal heuristic function value of the neighbors
    */
  def incrementLimit(availableLimits: SortedSet[Int] ): Option[Int] = Option(availableLimits.min)

  /**
    * one recursion of IDA* algorithm
    * @param first first node of the searched path
    * @param nodes last node of the searched path
    * @param cost cost of the current path
    * @param limit IDA* limit for the current recursion
    * @param flag true if the recursion starts with a new limit
    * @return current path,
    */
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
