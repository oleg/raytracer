package ray.tracer

class Color(val red: Double, val green: Double, val blue: Double) {

  def +(other: Color): Color = zip(other, _ + _)

  def -(other: Color): Color = zip(other, _ - _)

  def *(other: Color): Color = zip(other, _ * _) //Hadamard product

  def *(scalar: Double): Color = map(_ * scalar)

  def asStr: String = f"$red%.0f $green%.0f $blue%.0f"

  def map(f: Double => Double): Color = new Color(f(red), f(green), f(blue))

  private def zip(other: Color, f: (Double, Double) => Double): Color =
    new Color(f(red, other.red), f(green, other.green), f(blue, other.blue))

  def ==~(other: Color)(implicit p: Precision[Double]): Boolean =
    p.approximatelyEqual(red, other.red) &&
      p.approximatelyEqual(green, other.green) &&
      p.approximatelyEqual(blue, other.blue)

  def canEqual(other: Any): Boolean = other.isInstanceOf[Color]

  override def equals(other: Any): Boolean = other match {
    case that: Color =>
      (that canEqual this) &&
        red == that.red &&
        green == that.green &&
        blue == that.blue
    case _ => false
  }

  override def hashCode: Int = (red, green, blue).##

  override def toString: String = s"Color($red $green $blue)"
}


object Color {
  def apply(red: Double, green: Double, blue: Double): Color = new Color(red, green, blue)

  val white: Color = Color(1, 1, 1)
  val black: Color = Color(0, 0, 0)
}
