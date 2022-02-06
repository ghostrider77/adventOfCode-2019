
object Day02a {
  import scala.annotation.tailrec

  def runProgram(program: Vector[Int]): Int = {
    @tailrec
    def loop(state: Vector[Int], position: Int): Int = {
      val instruction: Int = state(position)
      if (instruction == 1) {
        val sum: Int = state(state(position + 1)) + state(state(position + 2))
        loop(state.updated(state(position + 3), sum), position + 4)
      } else if (instruction == 2) {
        val prod: Int = state(state(position + 1)) * state(state(position + 2))
        loop(state.updated(state(position + 3), prod), position + 4)
      } else state.head
    }

    loop(program, position = 0)
  }

  def main(args: Array[String]): Unit = {
    val line: String = scala.io.Source.fromResource("input_02.txt").getLines().next()
    val program: Vector[Int] = line.split(",").map(_.toInt).toVector
    val modifiedProgram: Vector[Int] = program.patch(1, Vector(12, 2), 2)
    val result: Int = runProgram(modifiedProgram)
    println(result)
  }
}
