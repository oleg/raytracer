package ray

import org.scalatest.FunSuite

class MatrixTest extends FunSuite {

  test("A 0x0 matrix ought to have correct width and height") {
    val m = Matrix(Array())

    assert(m.width == 0)
    assert(m.height == 0)
  }

  test("A 1x1 matrix ought to have correct width and height") {
    val m = Matrix(Array(Array(5)))

    assert(m.width == 1)
    assert(m.height == 1)
  }

  test("A 2x2 matrix ought to have correct width and height") {
    val m = Matrix(DoubleArray(
      """
        |  1  |  2  |
        |  4  |  3  |
      """))

    assert(m.width == 2)
    assert(m.height == 2)
  }
  test("A 4x2 matrix ought to have correct width and height") {
    val m = Matrix(DoubleArray(
      """
        |  1   |  2   |  3   |  4  |
        |  1   |  2   |  3   |  4  |
      """))

    assert(m.width == 4)
    assert(m.height == 2)
  }

  test("A matrix ought to have columns of the same length") {
    val exception = intercept[IllegalArgumentException] {
      Matrix(Array(Array(1, 2), Array(3)))
    }
    assert(exception.getMessage == "Unequal columns lengths 1 != 2")
  }

  test("May have null") { //todo: think about it
    val m = Matrix(Array(
      Array(1, 2, 3),
      Array(null.asInstanceOf[Double], null.asInstanceOf[Double], 3)))
    assert(m.width == 3)
    assert(m.height == 2)
  }

  test("Multiplication fails for incompatible matrices (left width) != (right height)") {
    val exception = intercept[IllegalArgumentException] {
      val ma = Matrix(DoubleArray(
        """
          |  1   |  2   |  3   |  4  |
        """))
      val mb = Matrix(DoubleArray(
        """
          |  1  |
          |  2  |
          |  3  |
        """))
      ma * mb
    }
    assert(exception.getMessage == "Multiplication is not possible (left width) 4 != 3 (right height)")

  }

  test("Constructing and inspecting a 4x4 matrix") {
    val m = Matrix(DoubleArray(
      """
        |  1   |  2   |  3   |  4   |
        |  5.5 |  6.5 |  7.5 |  8.5 |
        |  9   | 10   | 11   | 12   |
        | 13.5 | 14.5 | 15.5 | 16.5 |
      """))

    assert(m(0, 0) == 1)
    assert(m(0, 3) == 4)
    assert(m(1, 0) == 5.5)
    assert(m(1, 2) == 7.5)
    assert(m(2, 2) == 11)
    assert(m(3, 0) == 13.5)
    assert(m(3, 2) == 15.5)
  }

  test("A 2x2 matrix ought to be representable") {
    val m = Matrix(DoubleArray(
      """
        | -3 |  5 |
        |  1 | -2 |
      """))

    assert(m(0, 0) == -3)
    assert(m(0, 1) == 5)
    assert(m(1, 0) == 1)
    assert(m(1, 1) == -2)
  }

  test("A 3x3 matrix ought to be representable") {
    val m = Matrix(DoubleArray(
      """
        | -3 |  5 |  0 |
        |  1 | -2 | -7 |
        |  0 |  1 |  1 |
      """))

    assert(m(0, 0) == -3)
    assert(m(1, 1) == -2)
    assert(m(2, 2) == 1)
  }

  test("Matrix equality with identical matrices") {
    val ma = Matrix(DoubleArray(
      """
        | 1 | 2 | 3 | 4 |
        | 5 | 6 | 7 | 8 |
        | 9 | 8 | 7 | 6 |
        | 5 | 4 | 3 | 2 |
      """))
    val mb = Matrix(DoubleArray(
      """
        | 1 | 2 | 3 | 4 |
        | 5 | 6 | 7 | 8 |
        | 9 | 8 | 7 | 6 |
        | 5 | 4 | 3 | 2 |
      """))
    assert(ma == mb)
  }

  test("Matrix equality with different matrices") {
    val ma = Matrix(DoubleArray(
      """
        | 1 | 2 | 3 | 4 |
        | 5 | 6 | 7 | 8 |
        | 9 | 8 | 7 | 6 |
        | 5 | 4 | 3 | 2 |
      """))
    val mb = Matrix(DoubleArray(
      """
        | 2 | 3 | 4 | 5 |
        | 6 | 7 | 8 | 9 |
        | 8 | 7 | 6 | 5 |
        | 4 | 3 | 2 | 1 |
      """))
    assert(ma != mb)
  }

  test("Multiplying two matrices") {
    val ma = Matrix(DoubleArray(
      """
        | 1 | 2 | 3 | 4 |
        | 5 | 6 | 7 | 8 |
        | 9 | 8 | 7 | 6 |
        | 5 | 4 | 3 | 2 |
      """))

    val mb = Matrix(DoubleArray(
      """
        | -2 | 1 | 2 |  3 |
        |  3 | 2 | 1 | -1 |
        |  4 | 3 | 6 |  5 |
        |  1 | 2 | 7 |  8 |
      """))

    assert(ma * mb == Matrix(DoubleArray(
      """
        | 20|  22 |  50 |  48 |
        | 44|  54 | 114 | 108 |
        | 40|  58 | 110 | 102 |
        | 16|  26 |  46 |  42 |
      """)))
  }

  test("Multiplying two non square matrices ") {
    val ma = Matrix(DoubleArray(
      """
        | -2 | 1 |
        |  3 | 2 |
        |  4 | 3 |
      """))

    val mb = Matrix(DoubleArray(
      """
        | 1 | 2 | 3 | 4 |
        | 5 | 6 | 7 | 8 |
      """))

    assert(ma * mb == Matrix(DoubleArray(
      """
        |  3 |   2 |  1 |  0 |
        | 13 |  18 | 23 | 28 |
        | 19 |  26 | 33 | 40 |
      """)))
  }

  test("A matrix multiplied by a tuple") {
    val ma = Matrix(DoubleArray(
      """
        | 1 | 2 | 3 | 4 |
        | 2 | 4 | 4 | 2 |
        | 8 | 6 | 4 | 1 |
        | 0 | 0 | 0 | 1 |
      """))

    val b = Tuple(1, 2, 3, 1)

    assert(ma * b == Tuple(18, 24, 33, 1))
  }
}


object DoubleArray {

  def apply(str: String): Array[Array[Double]] =
    str.split("\n")
      .map(_.trim)
      .filterNot(_.isEmpty)
      .map(
        _.split("\\|")
          .map(_.trim)
          .filterNot(_.isEmpty)
          .map(_.toDouble))

}
