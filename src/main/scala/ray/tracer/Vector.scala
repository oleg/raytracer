package ray.tracer

case class Vector(x: Double, y: Double, z: Double) {

  def sum: Double = x + y + z //todo: val?

  def +(other: Vector): Vector = Vector(x + other.x, y + other.y, z + other.z)

  def -(other: Vector): Vector = Vector(x - other.x, y - other.y, z - other.z)

  def *(scalar: Double): Vector = Vector(x * scalar, y * scalar, z * scalar)

  def /(scalar: Double): Vector = Vector(x / scalar, y / scalar, z / scalar)

  def unary_-(): Vector = Vector(-x, -y, -z) // mergeScalar(0, (a, b) => b - a)

  def normalize: Vector = this / magnitude

  def magnitude: Double = math.sqrt(Vector(math.pow(x, 2), math.pow(y, 2), math.pow(z, 2)).sum) //todo val?

  def dot(other: Vector): Double = Vector(x * other.x, y * other.y, z * other.z).sum

  def cross(other: Vector) = Vector(
    y * other.z - z * other.y,
    z * other.x - x * other.z,
    x * other.y - y * other.x)

  def reflect(normal: Vector): Vector = {
    val thisInstance: Vector = this
    val vector: Vector = normal * 2 * (this dot normal)
    (thisInstance - vector)
  }

  def ==~(other: Vector)(implicit p: Precision[Double]): Boolean = {
    p.approximatelyEqual(x, other.x) &&
      p.approximatelyEqual(y, other.y) &&
      p.approximatelyEqual(z, other.z)
  }
}

object Vector {
  def apply(x: Double, y: Double, z: Double): Vector = new Vector(x, y, z)
}
