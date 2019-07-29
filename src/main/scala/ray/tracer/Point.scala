package ray.tracer

case class Point(x: Double, y: Double, z: Double) {

  def -(other: Point): Vector = Vector(x - other.x, y - other.y, z - other.z)

  def -(other: Vector): Point = Point(x - other.x, y - other.y, z - other.z)

  def +(other: Vector): Point = Point(x + other.x, y + other.y, z + other.z)

  def *(scalar: Double): Point = Point(x * scalar, y * scalar, z * scalar) //todo test me

  def /(scalar: Double): Point = Point(x / scalar, y / scalar, z / scalar) //todo test me

  def unary_-(): Point = Point(-x, -y, -z) //todo test me

  //TODO Implement via implicits!!!
  def ==~(other: Point): Boolean = {
    //(asList zip other.asList).forall(b => eql(b._1, b._2))

    approximatelyEqual(x, other.x) &&
      approximatelyEqual(y, other.y) &&
      approximatelyEqual(z, other.z)
  }

}

object Point {
  def apply(x: Double, y: Double, z: Double): Point = new Point(x, y, z)
}