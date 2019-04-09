package ray

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

  def apply(i: Int, j: Int): Double = matrix(i)(j)

  def canEqual(other: Any): Boolean = other.isInstanceOf[Matrix]

  override def equals(other: Any): Boolean = other match {
    case that: Matrix =>
      (that canEqual this) &&
        //        (matrix sameElements that.matrix)
        (matrix.deep == that.matrix.deep) //todo think about it
    case _ => false
  }

  override def hashCode(): Int = matrix.##

  override def toString = matrix.deep.toString
}

object Matrix {
  def apply(matrix: Array[Array[Double]]): Matrix = new Matrix(matrix)
}
