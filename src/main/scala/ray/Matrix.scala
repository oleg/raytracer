package ray

//TODO immutable?
class Matrix(private val matrix: Array[Array[Double]]) {

  val height: Int = matrix.length
  val width: Int = if (matrix.nonEmpty) matrix.map(_.length).max else 0

  matrix
    .map(_.length)
    .find(_ != width)
    .map(length => throw new IllegalArgumentException(s"Unequal columns lengths $length != $width"))

  //todo add size checks
  def *(other: Tuple): Tuple = { //todo :scary: fix me
    val m = this * Matrix(Array(Array(other.x), Array(other.y), Array(other.z), Array(other.w)))
    //todo: fix me
    Tuple(m(0, 0), m(1, 0), m(2, 0), m(3, 0))
  }

  def *(other: Matrix): Matrix = {
    if (width != other.height)
      throw new IllegalArgumentException(s"Multiplication is not possible (left width) $width != ${other.height} (right height)")

    val result = Array.fill(height, other.width)(0.0)

    for (row <- 0 until height) {
      for (col <- 0 until other.width) {
        result(row)(col) = (0 until width).map(i => matrix(row)(i) * other.matrix(i)(col)).sum
      }
    }
    new Matrix(result)
  }

  def transpose: Matrix = Matrix(matrix.transpose)

  def determinant: Double = { //todo test not square matrices
    if (width == 2 && height == 2) {
      matrix(0)(0) * matrix(1)(1) - matrix(0)(1) * matrix(1)(0) //todo fix me
    } else {
      var det = 0.0
      for (c <- 0 until height) {
        det += matrix(0)(c) * cofactor(0, c)
      }
      det
    }
  }

  def submatrix(row: Int, column: Int): Matrix = { //todo add test for 0x0
    val newMatrix = Array.ofDim[Double](height - 1, width - 1)
    var nr: Int = 0
    var nc: Int = 0
    for (r <- matrix.indices) {
      nc = 0
      if (r != row) {
        for (c <- matrix(r).indices) {
          if (c != column) {
            newMatrix(nr)(nc) = matrix(r)(c)
            nc += 1
          }
        }
        nr += 1
      }
    }
    Matrix(newMatrix)
  }

  def minor(row: Int, column: Int): Double = submatrix(row, column).determinant

  def cofactor(row: Int, column: Int): Double = {
    def f: Double => Double = if ((row + column) % 2 == 0) +_ else -_

    f(minor(row, column))
  }

  def isInvertible: Boolean = determinant != 0

  def inverse: Matrix = {
    //todo refactor
    val det: Double = determinant
    val inv = Array.ofDim[Double](height, width)
    for (r <- matrix.indices) {
      for (c <- matrix(r).indices) {
        inv(c)(r) = cofactor(r, c) / det
      }
    }
    Matrix(inv)
  }

  //TODO Implement via implicits!!!
  def !==~(other: Matrix): Boolean = {
    !(this ==~ other)
  }

  def ==~(other: Matrix): Boolean = {
    val epsilon = 0.00001

    def eql = (a: Double, b: Double) => math.abs(a - b) < epsilon

    height == other.height &&
      width == other.width &&
      (0 until height).forall(r =>
        (0 until width).forall(c => eql(matrix(r)(c), other.matrix(r)(c))))
  }

  def apply(i: Int, j: Int): Double = matrix(i)(j)

  def canEqual(other: Any): Boolean = other.isInstanceOf[Matrix]

  override def equals(other: Any): Boolean = other match {
    case that: Matrix =>
      (that canEqual this) &&
        (width == that.width && height == that.height) &&
        (matrix.deep == that.matrix.deep) //todo think about it: (matrix sameElements that.matrix)
    case _ => false
  }

  override def hashCode(): Int = matrix.##

  override def toString: String = {
    val leftPad = (s: String, len: Int) => " " * (len - s.length()) + s
    val max: Int = matrix.flatMap(x => x.map(_.toString.length)).max
    matrix.map(row => row.map(e => leftPad(e.toString, max)).mkString(" | ")).map(e => "| " + e + " |").mkString("\n")
  }
}

object Matrix {
  def apply(matrix: Array[Array[Double]]): Matrix = new Matrix(matrix)

  //TODO must be immutable
  def IdentityMatrix4x4: Matrix = Matrix(
    Array(
      Array(1, 0, 0, 0),
      Array(0, 1, 0, 0),
      Array(0, 0, 1, 0),
      Array(0, 0, 0, 1)))

  def TranslationMatrix4x4(x: Double, y: Double, z: Double): Matrix = {
    val m = IdentityMatrix4x4
    m.matrix(0)(3) = x
    m.matrix(1)(3) = y
    m.matrix(2)(3) = z
    m
  }

  def ScalingMatrix4x4(x: Double, y: Double, z: Double): Matrix = {
    val m = IdentityMatrix4x4
    m.matrix(0)(0) = x
    m.matrix(1)(1) = y
    m.matrix(2)(2) = z
    m
  }

  def RotationXMatrix4x4(radian: Double): Matrix = {
    val m = IdentityMatrix4x4
    val cosr = math.cos(radian)
    val sinr = math.sin(radian)
    m.matrix(1)(1) = cosr
    m.matrix(2)(2) = cosr
    m.matrix(1)(2) = -sinr
    m.matrix(2)(1) = sinr
    m
  }

  def RotationYMatrix4x4(radian: Double): Matrix = {
    val m = IdentityMatrix4x4
    val cosr = math.cos(radian)
    val sinr = math.sin(radian)
    m.matrix(0)(0) = cosr
    m.matrix(0)(2) = sinr
    m.matrix(2)(0) = -sinr
    m.matrix(2)(2) = cosr
    m
  }

  def RotationZMatrix4x4(radian: Double): Matrix = {
    val m = IdentityMatrix4x4
    val cosr = math.cos(radian)
    val sinr = math.sin(radian)
    m.matrix(0)(0) = cosr
    m.matrix(1)(1) = cosr
    m.matrix(0)(1) = -sinr
    m.matrix(1)(0) = cosr
    m
  }

  def ShearingMatrix4x4(xy: Double, xz: Double,
                        yx: Double, yz: Double,
                        zx: Double, zy: Double): Matrix = {

    val m = IdentityMatrix4x4
    m.matrix(0)(1) = xy
    m.matrix(0)(2) = xz
    m.matrix(1)(0) = yx
    m.matrix(1)(2) = yz
    m.matrix(2)(0) = zx
    m.matrix(2)(1) = zy
    m
  }

}
