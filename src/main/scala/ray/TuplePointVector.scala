package ray

class Tuple(val x: Double, val y: Double, val z: Double, val w: Double) {

  def sum: Double = x + y + z + w //todo: val?

  def +(other: Tuple): Tuple = merge(other, _ + _)

  def -(other: Tuple): Tuple = merge(other, _ - _)

  def *(scalar: Double): Tuple = mergeScalar(scalar, _ * _)

  def /(scalar: Double): Tuple = mergeScalar(scalar, _ / _)

  def unary_-(): Tuple = mergeScalar(0, (a, b) => b - a)

  def mergeScalar(scalar: Double, f: (Double, Double) => Double): Tuple =
    merge(new Tuple(scalar, scalar, scalar, scalar), f)

  def merge(other: Tuple, f: (Double, Double) => Double): Tuple =
    new Tuple(f(x, other.x), f(y, other.y), f(z, other.z), f(w, other.w))

  def canEqual(other: Any): Boolean = other.isInstanceOf[Tuple]

  override def equals(other: Any): Boolean = other match {
    case that: Tuple =>
      (that canEqual this) &&
        x == that.x &&
        y == that.y &&
        z == that.z &&
        w == that.w
    case _ => false
  }

  override def hashCode: Int = (x, y, z, w).##

  override def toString: String = s"Tuple $x $y $z $w"
}

class Point(x: Double, y: Double, z: Double) extends Tuple(x, y, z, 1.0) {
  override def toString: String = s"Point $x $y $z $w"
}

class Vector(x: Double, y: Double, z: Double) extends Tuple(x, y, z, 0.0) {

  //  val magnitude: Double = math.sqrt(math.pow(x, 2) + math.pow(y, 2) + math.pow(z, 2) + math.pow(w, 2))
  def magnitude: Double = math.sqrt(mergeScalar(2, math.pow).sum) //todo: val?

  def normalize: Vector = this / magnitude

  override def /(scalar: Double): Vector = new Vector(
    x / scalar,
    y / scalar,
    z / scalar)

  def dot(other: Vector): Double = new Vector(
    x * other.x,
    y * other.y,
    z * other.z).sum

  def cross(other: Vector) = new Vector(
    y * other.z - z * other.y,
    z * other.x - x * other.z,
    x * other.y - y * other.x)

  override def toString: String = s"Vector $x $y $z $w"
}

//objects
object Tuple {
  def apply(x: Double, y: Double, z: Double, w: Double): Tuple = new Tuple(x, y, z, w)
}

object Point {
  def apply(x: Double, y: Double, z: Double): Point = new Point(x, y, z)
}

object Vector {
  def apply(x: Double, y: Double, z: Double): Vector = new Vector(x, y, z)
}
