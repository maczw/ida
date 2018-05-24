package jps


import scala.collection.immutable.Map
import scala.collection.immutable.List
import scala.collection.immutable.Vector


object Graph {
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


  def main(args: Array[String]){
    println(neighborsMap)
  }
}
