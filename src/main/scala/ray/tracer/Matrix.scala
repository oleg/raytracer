package ray.tracer

/**
  * Matrix class support operations for matrices of any dimensions
  */
//TODO immutable?
class Matrix(private val points: Array[Array[Double]]) {

  val height: Int = points.length
  val width: Int = if (points.isEmpty) 0 else points(0).length

  //  points
  //    .find(_.length != width) //todo optimization
  //    .map(length => throw new IllegalArgumentException(s"Unequal columns lengths $length != $width"))

  def *(other: Matrix): Matrix = {
    if (width != other.height)
      throw new IllegalArgumentException(s"Multiplication is not possible (left width) $width != ${other.height} (right height)")

    val result = Array.ofDim[Double](height, other.width)
    for (row <- 0 until height) {
      for (col <- 0 until other.width) {
        result(row)(col) = (0 until width).map(i => points(row)(i) * other.points(i)(col)).sum
      }
    }
    Matrix(result)
  }

  def transpose: Matrix = Matrix(points.transpose)

  def isInvertible: Boolean = determinant != 0

  def inverse: Matrix = {
    //todo refactor
    val det: Double = determinant
    val inv = Array.ofDim[Double](height, width)
    for (r <- points.indices) {
      for (c <- points(r).indices) {
        inv(c)(r) = cofactor(r, c) / det
      }
    }
    Matrix(inv)
  }

  def determinant: Double = { //todo test not square matrices
    if (width == 2 && height == 2) {
      points(0)(0) * points(1)(1) - points(0)(1) * points(1)(0) //todo fix me
    } else {
      var det = 0.0
      for (c <- 0 until height) {
        det += points(0)(c) * cofactor(0, c)
      }
      det
    }
  }

  def cofactor(row: Int, column: Int): Double = {
    def sign: Double => Double = if ((row + column) % 2 == 0) +_ else -_

    sign(minor(row, column))
  }

  def minor(row: Int, column: Int): Double = submatrix(row, column).determinant

  def submatrix(row: Int, column: Int): Matrix = { //todo add test for 0x0
    val newMatrix = Array.ofDim[Double](height - 1, width - 1)
    var nr: Int = 0
    for (r <- points.indices) {
      if (r != row) {
        if (column == 0) {
          Array.copy(points(r), 1, newMatrix(nr), 0, width - 1)
        } else if (column == width) {
          Array.copy(points(r), 0, newMatrix(nr), 0, width - 1)
        } else {
          Array.copy(points(r), 0, newMatrix(nr), 0, column)
          Array.copy(points(r), column + 1, newMatrix(nr), column, width - column - 1)
        }
        nr += 1
      }
    }
    Matrix(newMatrix)
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
        (0 until width).forall(c => eql(points(r)(c), other.points(r)(c))))
  }

  def apply(i: Int, j: Int): Double = points(i)(j)

  def canEqual(other: Any): Boolean = other.isInstanceOf[Matrix]

  override def equals(other: Any): Boolean = other match {
    case that: Matrix =>
      (that canEqual this) &&
        (width == that.width && height == that.height) &&
        (points.deep == that.points.deep) //todo think about it: (matrix sameElements that.matrix)
    case _ => false
  }

  override def hashCode(): Int = points.##

  override def toString: String = {
    val leftPad = (s: String, len: Int) => " " * (len - s.length()) + s
    val max: Int = points.flatMap(x => x.map(_.toString.length)).max
    points.map(row => row.map(e => leftPad(e.toString, max)).mkString(" | ")).map(e => "| " + e + " |").mkString("\n")
  }
}

object Matrix {
  def apply(matrix: Array[Array[Double]]): Matrix = new Matrix(matrix)
}






