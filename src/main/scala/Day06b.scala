
object Day06b {
  import scala.annotation.tailrec

  private val StartNode: String = "YOU"
  private val EndNode: String = "SAN"

  private def readObjects(line: String): (String, String) = line.split("\\)").toList match {
    case List(a, b) => (a, b)
    case _ => throw new Exception("Cannot parse line.")
  }

  def calcNrOrbitalTransfers(tree: Map[String, List[String]]): Int = {
    @tailrec
    def loop(currentNodes: List[String], visitedNodes: Set[String], dist: Int): Int = {
      if (currentNodes.contains(EndNode)) dist - 2
      else {
        val nextNodes: List[String] =
          currentNodes.flatMap(node => tree.getOrElse(node, Nil)).filterNot(visitedNodes.contains)
        loop(nextNodes, visitedNodes ++ currentNodes, dist + 1)
      }
    }

    loop(List(StartNode), Set(), 0)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.fromResource("input_06.txt").getLines()
    val tree: Map[String, List[String]] =
      reader
        .map(readObjects)
        .flatMap{ case (a, b) => List((a, b), (b, a)) }
        .toList
        .groupMap{ case (a, _) => a }{ case (_, b) => b }
    val result: Int = calcNrOrbitalTransfers(tree)
    println(result)
  }
}
