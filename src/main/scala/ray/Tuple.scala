package ray

case class Tuple(x: Double, y: Double, z: Double, w: Double) {

  def isVector: Boolean = w == 0.0

  def isPoint: Boolean = w == 1.0

  def toVector: Tuple = copy(w = 0.0)

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

  def reflect(normal: Tuple): Tuple = this - normal * 2 * (this dot normal)

  private def mergeScalar(scalar: Double, f: (Double, Double) => Double): Tuple =
    new Tuple(f(x, scalar), f(y, scalar), f(z, scalar), f(w, scalar)) //merge(new Tuple(scalar, scalar, scalar, scalar), f)

  private def merge(other: Tuple, f: (Double, Double) => Double): Tuple =
    new Tuple(f(x, other.x), f(y, other.y), f(z, other.z), f(w, other.w))

  //TODO Implement via implicits!!!
  def ==~(other: Tuple): Boolean = {
    //(asList zip other.asList).forall(b => eql(b._1, b._2))
    val epsilon = 0.00001

    def eql = (a: Double, b: Double) => math.abs(a - b) < epsilon

    eql(x, other.x) &&
      eql(y, other.y) &&
      eql(z, other.z) &&
      eql(w, other.w)
  }

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
