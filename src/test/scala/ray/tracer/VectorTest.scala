package ray.tracer

import org.scalatest.funsuite.AnyFunSuite

class VectorTest extends AnyFunSuite {

  test("Vector is equal to itself") {
    val v1 = Vector(Double.NaN, Double.NaN, Double.NaN)

    assert(v1 == v1)
  }

  test("two Vectors are equal") {
    val v1 = Vector(1, 2, 3)
    val v2 = Vector(1, 2, 3)

    assert(v1 == v2)
  }

  test("two NaN Vectors are not equal") {
    val v1 = Vector(Double.NaN, Double.NaN, Double.NaN)
    val v2 = Vector(Double.NaN, Double.NaN, Double.NaN)

    assert(v1 != v2)
  }

}
