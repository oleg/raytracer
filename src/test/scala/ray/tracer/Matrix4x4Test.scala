package ray.tracer

import org.scalatest.funsuite.AnyFunSuite

class Matrix4x4Test extends AnyFunSuite {

  test("A 4x4 matrix ought to have 4 columns and 4 rows") {
    val exception = intercept[IllegalArgumentException] {
      Matrix4x4(Array(Array(1, 2), Array(3, 4)))
    }
    assert(exception.getMessage == "Expected 4x4 matrix, but found 2x2")
  }

  test("Constructing and inspecting a 4x4 matrix") {
    val m = Matrix4x4(DoubleArray(
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

  test("Matrix4x4 equality with identical matrices") {
    val ma = Matrix4x4(DoubleArray(
      """
        | 1 | 2 | 3 | 4 |
        | 5 | 6 | 7 | 8 |
        | 9 | 8 | 7 | 6 |
        | 5 | 4 | 3 | 2 |
      """))
    val mb = Matrix4x4(DoubleArray(
      """
        | 1 | 2 | 3 | 4 |
        | 5 | 6 | 7 | 8 |
        | 9 | 8 | 7 | 6 |
        | 5 | 4 | 3 | 2 |
      """))
    assert(ma ==~ mb)
  }

  test("Matrix4x4 equality with different matrices") {
    val ma = Matrix4x4(DoubleArray(
      """
        | 1 | 2 | 3 | 4 |
        | 5 | 6 | 7 | 8 |
        | 9 | 8 | 7 | 6 |
        | 5 | 4 | 3 | 2 |
      """))
    val mb = Matrix4x4(DoubleArray(
      """
        | 2 | 3 | 4 | 5 |
        | 6 | 7 | 8 | 9 |
        | 8 | 7 | 6 | 5 |
        | 4 | 3 | 2 | 1 |
      """))
    assert(ma !==~ mb)
  }

  test("Multiplying two matrices") {
    val ma = Matrix4x4(DoubleArray(
      """
        | 1 | 2 | 3 | 4 |
        | 5 | 6 | 7 | 8 |
        | 9 | 8 | 7 | 6 |
        | 5 | 4 | 3 | 2 |
      """))

    val mb = Matrix4x4(DoubleArray(
      """
        | -2 | 1 | 2 |  3 |
        |  3 | 2 | 1 | -1 |
        |  4 | 3 | 6 |  5 |
        |  1 | 2 | 7 |  8 |
      """))

    assert(ma * mb ==~ Matrix4x4(DoubleArray(
      """
        | 20|  22 |  50 |  48 |
        | 44|  54 | 114 | 108 |
        | 40|  58 | 110 | 102 |
        | 16|  26 |  46 |  42 |
      """)))
  }

  test("A matrix multiplied by a tuple") {
    val ma = Matrix4x4(DoubleArray(
      """
        | 1 | 2 | 3 | 4 |
        | 2 | 4 | 4 | 2 |
        | 8 | 6 | 4 | 1 |
        | 0 | 0 | 0 | 1 |
      """))

    val b = Point(1, 2, 3)

    assert(ma * b ==~ Point(18, 24, 33))
  }

  test("Multiplying a matrix by the identity matrix") {
    val ma = Matrix4x4(DoubleArray(
      """
        | 0 | 1 |  2 |  4 |
        | 1 | 2 |  4 |  8 |
        | 2 | 4 |  8 | 16 |
        | 4 | 8 | 16 | 32 |
      """))

    assert(ma * Matrix4x4.Identity ==~ ma)
  }

  test("Multiplying the identity matrix by a tuple") {
    val a = Vector(1, 2, 3)

    assert(Matrix4x4.Identity * a ==~ a)
  }

  test("Transposing a matrix") {
    val ma = Matrix4x4(DoubleArray(
      """
        | 0 | 9 | 3 | 0 |
        | 9 | 8 | 0 | 8 |
        | 1 | 8 | 5 | 3 |
        | 0 | 0 | 5 | 8 |
      """))

    assert(ma.transpose ==~ Matrix4x4(DoubleArray(
      """
        | 0 | 9 | 1 | 0 |
        | 9 | 8 | 8 | 0 |
        | 3 | 0 | 5 | 5 |
        | 0 | 8 | 3 | 8 |
      """)))
  }

  test("Transposing the identity matrix") {
    assert(Matrix4x4.Identity.transpose ==~ Matrix4x4.Identity)
  }

  test("Calculating the inverse of a matrix") {
    val ma = Matrix4x4(DoubleArray(
      """
        | -5 |  2 |  6 | -8 |
        |  1 | -5 |  1 |  8 |
        |  7 |  7 | -6 | -7 |
        |  1 | -3 |  7 |  4 |
      """))

    val mb = ma.inverse

    assert(mb(3, 2) == (-160.0 / 532.0))
    assert(mb(2, 3) == (105.0 / 532.0))
    assert(mb ==~ Matrix4x4(DoubleArray(
      """
        |  0.21804 |  0.45112 |  0.24060 | -0.04511 |
        | -0.80827 | -1.45676 | -0.44360 |  0.52067 |
        | -0.07894 | -0.22368 | -0.05263 |  0.19736 |
        | -0.52255 | -0.81390 | -0.30075 |  0.30639 |
      """)))
  }

  test("Calculating the inverse of another matrix") {
    val ma = Matrix4x4(DoubleArray(
      """
        |  8 | -5 |  9 |  2 |
        |  7 |  5 |  6 |  1 |
        | -6 |  0 |  9 |  6 |
        | -3 |  0 | -9 | -4 |
      """))

    val mb = ma.inverse

    assert(mb ==~ Matrix4x4(DoubleArray(
      """
        | -0.15384 | -0.15384 | -0.28205 | -0.53846 |
        | -0.07692 |  0.12307 |  0.02564 |  0.03076 |
        |  0.35897 |  0.35897 |  0.43589 |  0.92307 |
        | -0.69230 | -0.69230 | -0.76923 | -1.92307 |
      """)))
  }

  test("Calculating the inverse of a third matrix") {
    val ma = Matrix4x4(DoubleArray(
      """
        |  9 |  3 |  0 |  9 |
        | -5 | -2 | -6 | -3 |
        | -4 |  9 |  6 |  4 |
        | -7 |  6 |  6 |  2 |
      """))

    val mb = ma.inverse

    assert(mb ==~ Matrix4x4(DoubleArray(
      """
        | -0.04074 | -0.07777 |  0.14444 | -0.22222 |
        | -0.07777 |  0.03333 |  0.36666 | -0.33333 |
        | -0.02901 | -0.14629 | -0.10925 |  0.12962 |
        |  0.17777 |  0.06666 | -0.26666 |  0.33333 |
      """)))
  }

  test("Multiplying a product by its inverse") {
    val ma = Matrix4x4(DoubleArray(
      """
        |  3 | -9 |  7 |  3 |
        |  3 | -8 |  2 | -9 |
        | -4 |  4 |  4 |  1 |
        | -6 |  5 | -1 |  1 |
      """))

    val mb = Matrix4x4(DoubleArray(
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
