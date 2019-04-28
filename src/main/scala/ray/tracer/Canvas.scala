package ray.tracer

case class Canvas(width: Int, height: Int, initialColor: Color = Color(0, 0, 0)) {
  private val pixels: Array[Array[Color]] = Array.fill(width, height)(initialColor)

  def toPpm: String = {
    ppmHeader + ppmLines + "\n"
  }

  def update(i: Int, j: Int, color: Color): Unit =
    if ((0 until width).contains(i) &&
      (0 until height).contains(j)) {
      pixels(i)(j) = color
    } else {
      println(s"unexpected params $i, $j")
    }

  def apply(i: Int, j: Int): Color = pixels(i)(j)

  private def ppmLines = pixels.transpose.map(ppmLine).flatMap(splitLongLines).mkString("\n")

  //todo refactor
  private def splitLongLines(str: String): List[String] =
    if (str.length() <= 70) {
      List(str)
    } else {
      val space = str.lastIndexWhere(_ == ' ', 70)
      List(str.slice(0, space)) ++ splitLongLines(str.slice(space + 1, str.length))
    }

  private def ppmLine(row: Array[Color]) =
    row
      .map(c => c * 255)
      .map(c => c.map(math.ceil))
      .map(c => c.map(math.min(_, 255)))
      .map(c => c.map(math.max(_, 0)))
      .map(_.asStr)
      .mkString(" ")

  private def ppmHeader = s"P3\n$width $height\n255\n"
}
