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
    assert(ma ==~ mb)
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
    assert(ma !==~ mb)
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

    assert(ma * mb ==~ Matrix(DoubleArray(
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

    assert(ma * mb ==~ Matrix(DoubleArray(
      """
        |  3 |   2 |  1 |  0 |
        | 13 |  18 | 23 | 28 |
        | 19 |  26 | 33 | 40 |
      """)))
  }

  test("Transposing a matrix") {
    val ma = Matrix(DoubleArray(
      """
        | 0 | 9 | 3 | 0 |
        | 9 | 8 | 0 | 8 |
        | 1 | 8 | 5 | 3 |
        | 0 | 0 | 5 | 8 |
      """))

    assert(ma.transpose ==~ Matrix(DoubleArray(
      """
        | 0 | 9 | 1 | 0 |
        | 9 | 8 | 8 | 0 |
        | 3 | 0 | 5 | 5 |
        | 0 | 8 | 3 | 8 |
      """)))
  }

  test("Calculating the determinant of a 2x2 matrix") {
    val ma = Matrix(DoubleArray(
      """
        |  1 | 5 |
        | -3 | 2 |
      """))

    assert(ma.determinant == 17)
  }

  test("A submatrix of a 3x3 matrix is a 2x2 matrix") {
    val ma = Matrix(DoubleArray(
      """
        |  1 | 5 |  0 |
        | -3 | 2 |  7 |
        |  0 | 6 | -3 |
      """))
    assert(ma.submatrix(0, 2) ==~ Matrix(DoubleArray(
      """
        | -3 | 2 |
        |  0 | 6 |
      """
    )))
  }

  test("A submatrix of a 4x4 matrix is a 3x3 matrix") {
    val ma = Matrix(DoubleArray(
      """
        | -6 |  1 |  1 |  6 |
        | -8 |  5 |  8 |  6 |
        | -1 |  0 |  8 |  2 |
        | -7 |  1 | -1 |  1 |
      """))
    assert(ma.submatrix(2, 1) ==~ Matrix(DoubleArray(
      """
        | -6 |  1 | 6 |
        | -8 |  8 | 6 |
        | -7 | -1 | 1 |
      """
    )))
  }

  test("Calculating a minor of a 3x3 matrix") {
    val ma = Matrix(DoubleArray(
      """
        |  3 |  5 |  0 |
        |  2 | -1 | -7 |
        |  6 | -1 |  5 |
      """))

    val mb = ma.submatrix(1, 0)
    assert(mb.determinant == 25)
    assert(ma.minor(1, 0) == 25)
  }

  test("Calculating a cofactor of a 3x3 matrix") {
    val ma = Matrix(DoubleArray(
      """
        |  3 |  5 |  0 |
        |  2 | -1 | -7 |
        |  6 | -1 |  5 |
      """))

    assert(ma.minor(0, 0) == -12)
    assert(ma.cofactor(0, 0) == -12)
    assert(ma.minor(1, 0) == 25)
    assert(ma.cofactor(1, 0) == -25)
  }

  test("Calculating the determinant of a 3x3 matrix") {
    val ma = Matrix(DoubleArray(
      """
        |  1 |  2 |  6 |
        | -5 |  8 | -4 |
        |  2 |  6 |  4 |
      """))

    assert(ma.cofactor(0, 0) == 56)
    assert(ma.cofactor(0, 1) == 12)
    assert(ma.cofactor(0, 2) == -46)
    assert(ma.determinant == -196)
  }

  test("Calculating the determinant of a 4x4 matrix") {
    val ma = Matrix(DoubleArray(
      """
        | -2 | -8 |  3 |  5 |
        | -3 |  1 |  7 |  3 |
        |  1 |  2 | -9 |  6 |
        | -6 |  7 |  7 | -9 |
      """))

    assert(ma.cofactor(0, 0) == 690)
    assert(ma.cofactor(0, 1) == 447)
    assert(ma.cofactor(0, 2) == 210)
    assert(ma.cofactor(0, 3) == 51)
    assert(ma.determinant == -4071)
  }

  test("Testing an invertible matrix for invertibility") {
    val ma = Matrix(DoubleArray(
      """
        |  6 |  4 |  4 |  4 |
        |  5 |  5 |  7 |  6 |
        |  4 | -9 |  3 | -7 |
        |  9 |  1 |  7 | -6 |
      """))

    assert(ma.determinant == -2120)
    assert(ma.isInvertible)
  }

  test("Testing a noninvertible matrix for invertibility") {
    val ma = Matrix(DoubleArray(
      """
        | -4 |  2 | -2 | -3 |
        |  9 |  6 |  2 |  6 |
        |  0 | -5 |  1 | -5 |
        |  0 |  0 |  0 |  0 |
      """))

    assert(ma.determinant == 0)
    assert(!ma.isInvertible)
  }

  test("Calculating the inverse of a matrix") {
    val ma = Matrix(DoubleArray(
      """
        | -5 |  2 |  6 | -8 |
        |  1 | -5 |  1 |  8 |
        |  7 |  7 | -6 | -7 |
        |  1 | -3 |  7 |  4 |
      """))

    val mb = ma.inverse

    assert(ma.cofactor(2, 3) == -160)
    assert(mb(3, 2) == (-160.0 / 532.0))
    assert(ma.cofactor(3, 2) == 105)
    assert(mb(2, 3) == (105.0 / 532.0))
    assert(mb ==~ Matrix(DoubleArray(
      """
        |  0.21804 |  0.45112 |  0.24060 | -0.04511 |
        | -0.80827 | -1.45676 | -0.44360 |  0.52067 |
        | -0.07894 | -0.22368 | -0.05263 |  0.19736 |
        | -0.52255 | -0.81390 | -0.30075 |  0.30639 |
      """)))
  }

  test("Calculating the inverse of another matrix") {
    val ma = Matrix(DoubleArray(
      """
        |  8 | -5 |  9 |  2 |
        |  7 |  5 |  6 |  1 |
        | -6 |  0 |  9 |  6 |
        | -3 |  0 | -9 | -4 |
      """))

    val mb = ma.inverse

    assert(mb ==~ Matrix(DoubleArray(
      """
        | -0.15384 | -0.15384 | -0.28205 | -0.53846 |
        | -0.07692 |  0.12307 |  0.02564 |  0.03076 |
        |  0.35897 |  0.35897 |  0.43589 |  0.92307 |
        | -0.69230 | -0.69230 | -0.76923 | -1.92307 |
      """)))
  }

  test("Calculating the inverse of a third matrix") {
    val ma = Matrix(DoubleArray(
      """
        |  9 |  3 |  0 |  9 |
        | -5 | -2 | -6 | -3 |
        | -4 |  9 |  6 |  4 |
        | -7 |  6 |  6 |  2 |
      """))

    val mb = ma.inverse

    assert(mb ==~ Matrix(DoubleArray(
      """
        | -0.04074 | -0.07777 |  0.14444 | -0.22222 |
        | -0.07777 |  0.03333 |  0.36666 | -0.33333 |
        | -0.02901 | -0.14629 | -0.10925 |  0.12962 |
        |  0.17777 |  0.06666 | -0.26666 |  0.33333 |
      """)))
  }

  test("Multiplying a product by its inverse") {
    val ma = Matrix(DoubleArray(
      """
        |  3 | -9 |  7 |  3 |
        |  3 | -8 |  2 | -9 |
        | -4 |  4 |  4 |  1 |
        | -6 |  5 | -1 |  1 |
      """))

    val mb = Matrix(DoubleArray(
      """
        |  8 |  2 |  2 |  2 |
        |  3 | -1 |  7 |  0 |
        |  7 |  0 |  5 |  4 |
        |  6 | -2 |  0 |  5 |
      """))

    val mc = ma * mb
    assert(mc * mb.inverse ==~ ma)
  }

}
