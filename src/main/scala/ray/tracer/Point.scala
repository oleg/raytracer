package ray.tracer

case class Point(x: Double, y: Double, z: Double) {

  def -(other: Point): Vector = Vector(x - other.x, y - other.y, z - other.z)

  def -(other: Vector): Point = Point(x - other.x, y - other.y, z - other.z)

  def +(other: Vector): Point = Point(x + other.x, y + other.y, z + other.z)

  def *(scalar: Double): Point = Point(x * scalar, y * scalar, z * scalar) //todo test me

  def /(scalar: Double): Point = Point(x / scalar, y / scalar, z / scalar) //todo test me

  def unary_- : Point = Point(-x, -y, -z) //todo test me

  def ==~(other: Point)(using p: Precision[Double]): Boolean = {
    p.approximatelyEqual(x, other.x) &&
      p.approximatelyEqual(y, other.y) &&
      p.approximatelyEqual(z, other.z)
  }

}

object Point {
  def apply(x: Double, y: Double, z: Double): Point = new Point(x, y, z)
}