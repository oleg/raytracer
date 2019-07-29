package ray.tracer

case class Point(x: Double, y: Double, z: Double) {

  def isVector: Boolean = false

  def isPoint: Boolean = true

  def asTuple: Tuple = new Tuple(x, y, z, 1) //todo:delete me

  def sum: Double = x + y + z + 1 //todo: val?

  def +(other: Vector): Point = new Point(x + other.x, y + other.y, z + other.z)

  def +(other: Tuple): Tuple = merge(other, _ + _)

  def -(other: Point): Vector = new Vector(x - other.x, y - other.y, z - other.z)

  def -(other: Vector): Point = new Point(x - other.x, y - other.y, z - other.z)

  def *(scalar: Double): Tuple = mergeScalar(scalar, _ * _)

  def /(scalar: Double): Tuple = mergeScalar(scalar, _ / _)

  def unary_-(): Tuple = Tuple(-x, -y, -z, -1) // mergeScalar(0, (a, b) => b - a)

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

  //  def reflect(normal: Tuple): Tuple = {
  //    val thisInstance: Point = this
  //    val tuple: Tuple = normal * 2 * (this dot normal)
  //    thisInstance - tuple
  //  }

  private def mergeScalar(scalar: Double, f: (Double, Double) => Double): Tuple =
    new Tuple(f(x, scalar), f(y, scalar), f(z, scalar), f(1, scalar)) //merge(new Tuple(scalar, scalar, scalar, scalar), f)

  private def merge(other: Tuple, f: (Double, Double) => Double): Tuple =
    new Tuple(f(x, other.x), f(y, other.y), f(z, other.z), f(1, other.w))

  //TODO Implement via implicits!!!
  def ==~(other: Point): Boolean = {
    //(asList zip other.asList).forall(b => eql(b._1, b._2))

    approximatelyEqual(x, other.x) &&
      approximatelyEqual(y, other.y) &&
      approximatelyEqual(z, other.z)
  }

}

case class Vector(x: Double, y: Double, z: Double) {

  def asTuple: Tuple = new Tuple(x, y, z, 0)

  def isVector: Boolean = true

  def isPoint: Boolean = false

  def toVector: Vector = this

  def sum: Double = x + y + z //todo: val?

  def +(other: Tuple): Tuple = merge(other, _ + _)

  def +(other: Vector): Vector = new Vector(x + other.x, y + other.y, z + other.z)

  def -(other: Vector): Vector = new Vector(x - other.x, y - other.y, z - other.z)

  def *(scalar: Double): Vector = new Vector(x * scalar, y * scalar, z * scalar) // (scalar, _ * _)

  def /(scalar: Double): Vector = new Vector(x / scalar, y / scalar, z / scalar) //merge(new Tuple(scalar, scalar, scalar, scalar), f) mergeScalar(scalar, _ / _)

  def unary_-(): Vector = Vector(-x, -y, -z) // mergeScalar(0, (a, b) => b - a)

  //only for vectors
  def magnitude: Double = math.sqrt(mergeScalar(2, math.pow).sum) //todo: val?

  //only for vectors
  def normalize: Vector = this / magnitude

  //only for vectors
  def dot(other: Vector): Double = new Vector(x * other.x, y * other.y, z * other.z).sum

  //only for vectors
  def cross(other: Vector) = new Vector(
    y * other.z - z * other.y,
    z * other.x - x * other.z,
    x * other.y - y * other.x)

  def reflect(normal: Vector): Vector = {
    val thisInstance: Vector = this
    val vector: Vector = normal * 2 * (this dot normal)
    (thisInstance - vector)
  }

  private def mergeScalar(scalar: Double, f: (Double, Double) => Double): Tuple =
    new Tuple(f(x, scalar), f(y, scalar), f(z, scalar), f(0, scalar)) //merge(new Tuple(scalar, scalar, scalar, scalar), f)

  private def merge(other: Tuple, f: (Double, Double) => Double): Tuple =
    new Tuple(f(x, other.x), f(y, other.y), f(z, other.z), f(0, other.w))

  //TODO Implement via implicits!!!
  def ==~(other: Vector): Boolean = {
    //(asList zip other.asList).forall(b => eql(b._1, b._2))

    approximatelyEqual(x, other.x) &&
      approximatelyEqual(y, other.y) &&
      approximatelyEqual(z, other.z)
  }

}

case class Tuple(x: Double, y: Double, z: Double, w: Double) {

  def isVector: Boolean = w == 0.0

  def isPoint: Boolean = w == 1.0

  def toPoint: Point = new Point(x, y, z)

  def toVector: Vector = new Vector(x, y, z)

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

    approximatelyEqual(x, other.x) &&
      approximatelyEqual(y, other.y) &&
      approximatelyEqual(z, other.z) &&
      approximatelyEqual(w, other.w)
  }

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
