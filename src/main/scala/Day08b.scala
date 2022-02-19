
object Day08b {
  def decodeImage(imageData: String, nrPixelsPerLayer: Int): String = {
    def findPixel(pxs: List[Char]): Char = pxs.find(_ != '2') match {
      case Some(p) => p
      case None => throw new Exception("Only transparent pixels can be found on all layers for this pixel.")
    }

    val pixels: List[List[Char]] = imageData.grouped(nrPixelsPerLayer).toList.transpose
    pixels.map(findPixel).mkString
  }

  def main(args: Array[String]): Unit = {
    val data: String = scala.io.Source.fromResource("input_08.txt").getLines().next()
    val height: Int = 6
    val width: Int = 25
    val nrPixelsPerLayer: Int = height * width
    val result: String = decodeImage(data, nrPixelsPerLayer)
    println(result.map(c => if (c == '1') "#" else " ").grouped(width).map(_.mkString).mkString("\n"))
  }
}
