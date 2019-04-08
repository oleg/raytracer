package ray

class Tuple(val x: Double, val y: Double, val z: Double, val w: Double) {

  def isVector: Boolean = w == 0.0

  def isPoint: Boolean = w == 1.0

  def sum: Double = x + y + z + w //todo: val?

  def +(other: Tuple): Tuple = merge(other, _ + _)

  def -(other: Tuple): Tuple = merge(other, _ - _)

  def *(scalar: Double): Tuple = mergeScalar(scalar, _ * _)

  def /(scalar: Double): Tuple = mergeScalar(scalar, _ / _)

  def unary_-(): Tuple = Tuple(-x, -y, -z, -w) // mergeScalar(0, (a, b) => b - a)

  //only for vectors
  def magnitude: Double = math.sqrt(mergeScalar(2, math.pow).sum) //todo: val?

  //only for vectors
  def normalize: Tuple = this / magnitude

  //only for vectors
  def dot(other: Tuple): Double = merge(other, _ * _).sum

  //only for vectors
  def cross(other: Tuple) = new Tuple(
    y * other.z - z * other.y,
    z * other.x - x * other.z,
    x * other.y - y * other.x,
    0.0)


  private def mergeScalar(scalar: Double, f: (Double, Double) => Double): Tuple =
    new Tuple(f(x, scalar), f(y, scalar), f(z, scalar), f(w, scalar)) //merge(new Tuple(scalar, scalar, scalar, scalar), f)

  private def merge(other: Tuple, f: (Double, Double) => Double): Tuple =
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

  override def toString: String = s"Tuple($x $y $z $w)"
}

//objects
object Tuple {
  def apply(x: Double, y: Double, z: Double, w: Double): Tuple = new Tuple(x, y, z, w)
}

object Point {
  def apply(x: Double, y: Double, z: Double): Tuple = new Tuple(x, y, z, 1.0)
}

object Vector {
  def apply(x: Double, y: Double, z: Double): Tuple = new Tuple(x, y, z, 0.0)
}
