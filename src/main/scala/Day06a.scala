
object Day06a {
  import scala.annotation.tailrec
  private val Root: String = "COM"

  private def readObjects(line: String): (String, String) = line.split("\\)").toList match {
    case List(a, b) => (a, b)
    case _ => throw new Exception("Cannot parse line.")
  }

  def calcNrOrbits(tree: Map[String, List[String]]): Int = {
    @tailrec
    def loop(acc: Int, depth: Int, nodes: List[String]): Int = nodes match {
      case Nil => acc
      case _ =>
        val children: List[String] = nodes.flatMap(node => tree.getOrElse(node, Nil))
        loop(acc + depth * children.length, depth + 1, children)
    }
    loop(acc = 0, depth = 1, List(Root))
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.fromResource("input_06.txt").getLines()
    val tree: Map[String, List[String]] =
      reader.map(readObjects).toList.groupMap{ case (a, _) => a }{ case (_, b) => b }
    val result: Int = calcNrOrbits(tree)
    println(result)
  }
}
