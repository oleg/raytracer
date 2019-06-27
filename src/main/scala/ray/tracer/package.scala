package ray

package object tracer {

  //todo make it implicit
  val EPSILON: Double = 0.00001

  val Sqrt2: Double = math.sqrt(2)

  val Sqrt2Div2: Double = math.sqrt(2) / 2.0

  def approximatelyEqual(a: Double, b: Double): Boolean = math.abs(a - b) < EPSILON

  def sqr(a: Double): Double = math.pow(a, 2)

  //  type Point = Tuple
  //  type Vector = Tuple
}
