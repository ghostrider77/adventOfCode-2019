
object Day01a {
  def fuelRequirement(mass: Int): Int = mass / 3 - 2

  def main(args: Array[String]): Unit = {
    val lines: Iterator[String] = scala.io.Source.fromResource("input_01.txt").getLines()
    val masses: List[Int] = lines.map(_.toInt).toList
    val result: Int = masses.map(fuelRequirement).sum
    println(result)
  }
}
