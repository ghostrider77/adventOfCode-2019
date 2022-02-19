
object Day08a {
  def calcProduct(imageData: String, height: Int, width: Int): Int = {
    val nrPixelsPerLayer: Int = height * width
    val layerWithFewestZeroes: String = imageData.grouped(nrPixelsPerLayer).minBy(_.count(_ == '0'))
    layerWithFewestZeroes.count(_ == '1') * layerWithFewestZeroes.count(_ == '2')
  }

  def main(args: Array[String]): Unit = {
    val data: String = scala.io.Source.fromResource("input_08.txt").getLines().next()
    val height: Int = 6
    val width: Int = 25
    val result: Int = calcProduct(data, height, width)
    println(result)
  }
}
