package practice.test
object Dijkstra {
 
  type Path[Key] = (Double, List[Key])
 
  def Dijkstra[Key](lookup: Map[Key, List[(Double, Key)]], fringe: List[Path[Key]], dest: Key, visited: Set[Key]): Path[Key] = fringe match {
    case (dist, path) :: fringe_rest => path match {case key :: path_rest =>
      if (key == dest) (dist, path.reverse)
      else {
        val paths = lookup(key).flatMap {case (d, key) => if (!visited.contains(key)) List((dist + d, key :: path)) else Nil}
        val sorted_fringe = (paths ++ fringe_rest).sortWith {case ((d1, _), (d2, _)) => d1 < d2}
        Dijkstra(lookup, sorted_fringe, dest, visited + key)
      }
    }
    case Nil => (0, List())
  }
 
  def main(x: Array[String]): Unit = {
    val lookup = Map(
      "A" -> List((5.0, "B"), (5.0, "D"), (7.0, "E")),
      "B" -> List((4.0, "C")),
      "C" -> List((8.0, "D"), (2.0, "E")),
      "D" -> List((8.0, "C"), (6.0, "E")),
      "E" -> List((3.0, "B"))
    )
    val res = Dijkstra[String](lookup, List((0, List("A","D"))), "C", Set())
    println(res)
  }
}