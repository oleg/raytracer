package ray.tracer

/**
  * Matrix4x4 class support and specific for 4x4 matrices operations
  */
class Matrix4x4(private val matrix: Matrix) {
  if (matrix.width != 4 || matrix.height != 4)
    throw new IllegalArgumentException(s"Expected 4x4 matrix, but found ${matrix.height}x${matrix.width}")

  def this(array: Array[Array[Double]]) {
    this(Matrix(array))
  }

  def *(other: Matrix4x4): Matrix4x4 = new Matrix4x4(matrix * other.matrix)

  //todo add size checks
  def *(other: Tuple): Tuple = { //todo :scary: fix me
    val m = this.matrix * Matrix(Array(Array(other.x), Array(other.y), Array(other.z), Array(other.w)))
    //todo: fix me
    Tuple(m(0, 0), m(1, 0), m(2, 0), m(3, 0))
  }

  //TODO Implement via implicits!!!
  def !==~(other: Matrix4x4): Boolean = !(this ==~ other)

  def ==~(other: Matrix4x4): Boolean = matrix ==~ other.matrix

  def inverse: Matrix4x4 = new Matrix4x4(matrix.inverse)

  def transpose: Matrix4x4 = new Matrix4x4(matrix.transpose)

  def translate(x: Double, y: Double, z: Double): Matrix4x4 = Matrix4x4.Translation(x, y, z) * this

  def scale(x: Double, y: Double, z: Double): Matrix4x4 = Matrix4x4.Scaling(x, y, z) * this

  def rotateX(radian: Double): Matrix4x4 = Matrix4x4.RotationX(radian) * this

  def rotateY(radian: Double): Matrix4x4 = Matrix4x4.RotationY(radian) * this

  def rotateZ(radian: Double): Matrix4x4 = Matrix4x4.RotationZ(radian) * this

  def shear(xy: Double, xz: Double,
            yx: Double, yz: Double,
            zx: Double, zy: Double): Matrix4x4 = Matrix4x4.Shearing(xy, xz, yx, yz, zx, zy) * this

  def apply(i: Int, j: Int): Double = matrix(i, j)

  def canEqual(other: Any): Boolean = other.isInstanceOf[Matrix4x4]

  override def equals(other: Any): Boolean = other match {
    case that: Matrix4x4 =>
      (that canEqual this) &&
        (matrix == that.matrix)
    case _ => false
  }

  override def hashCode(): Int = matrix.##

  override def toString: String = matrix.toString
}

object Matrix4x4 {
  //  def apply(matrix: Matrix): Matrix4x4 = new Matrix4x4(matrix)
  def apply(matrix: Array[Array[Double]]): Matrix4x4 = new Matrix4x4(matrix)

  def Identity: Matrix4x4 =
    Matrix4x4(
      Array(
        Array(1, 0, 0, 0),
        Array(0, 1, 0, 0),
        Array(0, 0, 1, 0),
        Array(0, 0, 0, 1)))

  def Translation(x: Double, y: Double, z: Double): Matrix4x4 =
    Matrix4x4(
      Array(
        Array(1, 0, 0, x),
        Array(0, 1, 0, y),
        Array(0, 0, 1, z),
        Array(0, 0, 0, 1)))

  def Scaling(x: Double, y: Double, z: Double): Matrix4x4 =
    Matrix4x4(
      Array(
        Array(x, 0, 0, 0),
        Array(0, y, 0, 0),
        Array(0, 0, z, 0),
        Array(0, 0, 0, 1)))

  def RotationX(radian: Double): Matrix4x4 = {
    val cr = math.cos(radian)
    val sr = math.sin(radian)
    Matrix4x4(
      Array(
        Array(1, 0, 0, 0),
        Array(0, cr, -sr, 0),
        Array(0, sr, cr, 0),
        Array(0, 0, 0, 1)))
  }

  def RotationY(radian: Double): Matrix4x4 = {
    val cr = math.cos(radian)
    val sr = math.sin(radian)
    Matrix4x4(
      Array(
        Array(cr, 0, sr, 0),
        Array(0, 1, 0, 0),
        Array(-sr, 1, cr, 0),
        Array(0, 0, 0, 1)))

  }

  def RotationZ(radian: Double): Matrix4x4 = {
    val cr = math.cos(radian)
    val sr = math.sin(radian)
    Matrix4x4(
      Array(
        Array(cr, -sr, 0, 0),
        Array(sr, cr, 0, 0),
        Array(0, 0, 1, 0),
        Array(0, 0, 0, 1)))
  }

  def Shearing(xy: Double, xz: Double,
               yx: Double, yz: Double,
               zx: Double, zy: Double): Matrix4x4 =
    Matrix4x4(
      Array(
        Array(1, xy, xz, 0),
        Array(yx, 1, yz, 0),
        Array(zx, zy, 1, 0),
        Array(0, 0, 0, 1)))

  def viewTransform(from: Tuple, to: Tuple, up: Tuple): Matrix4x4 = {
    val forward = (to - from).normalize
    val left = forward.cross(up.normalize)
    val trueUp = left.cross(forward)

    val orientation = Matrix4x4(Array(
      Array(    left.x,     left.y,     left.z, 0),
      Array(  trueUp.x,   trueUp.y,   trueUp.z, 0),
      Array(-forward.x, -forward.y, -forward.z, 0),
      Array(         0,          0,          0, 1)))

    orientation * Translation(-from.x, -from.y, -from.z)
  }

}
