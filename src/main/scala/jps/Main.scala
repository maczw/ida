package scala.jps

/**
  * Starting point of the application
  */
object Main extends App{
  override def main(args: Array[String]) {
    println("Path and total cost: ", new GraphWithHeuristics(args(0)).idaStar())
  }
}
