
object Day02b {
  import scala.annotation.tailrec

  private def runProgram(program: Vector[Int]): Int = {
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

  def findInputs(program: Vector[Int], target: Int): Int = {
    (0 to 99)
      .flatMap(a => (0 to 99).map(b => (a, b)))
      .find{
        case (a, b) =>
          val modifiedProgram: Vector[Int] = program.patch(1, Vector(a, b), 2)
          runProgram(modifiedProgram) == target
      } match {
      case Some((noun, verb)) => 100 * noun + verb
      case None => throw new Exception("No solution exists for task.")
    }
  }

  def main(args: Array[String]): Unit = {
    val line: String = scala.io.Source.fromResource("input_02.txt").getLines().next()
    val program: Vector[Int] = line.split(",").map(_.toInt).toVector
    val target: Int = 19690720
    val result: Int = findInputs(program, target)
    println(result)
  }
}
