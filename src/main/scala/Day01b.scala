
object Day01b {
  import scala.annotation.tailrec

  def fuelRequirement(mass: Int): Int = {
    @tailrec
    def loop(totalFuel: Int, mass: Int): Int = {
      val fuel: Int = mass / 3 - 2
      if (fuel <= 0) totalFuel
      else loop(totalFuel + fuel, fuel)
    }
    loop(0, mass)
  }

  def main(args: Array[String]): Unit = {
    val lines: Iterator[String] = scala.io.Source.fromResource("input_01.txt").getLines()
    val masses: List[Int] = lines.map(_.toInt).toList
    val result: Int = masses.map(fuelRequirement).sum
    println(result)
  }
}
