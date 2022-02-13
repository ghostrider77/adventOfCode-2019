
object Day04b {
  def calcNrDifferentPasswords(a: Int, b: Int): Int = {
    def isValid(n: Int): Boolean = {
      val digits: List[Int] = n.toString.toList.map(_.asDigit)
      val adjacentDigits: List[(Int, Int)] = digits.sliding(2).collect{ case List(c1, c2) => (c1, c2) }.toList
      val init: List[(Int, Int)] = List((digits.head, 1))
      lazy val adjacentDigitCounts: List[(Int, Int)] =
        adjacentDigits.foldLeft(init){
          case (acc, (d1, d2)) =>
            if (d1 == d2) {
              val (d, c): (Int, Int) = acc.head
              (d, c + 1) :: acc.tail
            } else (d2, 1) :: acc
        }
      adjacentDigits.forall{ case (d1, d2) => d1 <= d2 } && adjacentDigitCounts.exists{ case (_, c) => c == 2 }
    }

    (a to b).count(isValid)
  }

  def main(args: Array[String]): Unit = {
    val a: Int = 193651
    val b: Int = 649729
    val result: Int = calcNrDifferentPasswords(a, b)
    println(result)
  }
}
