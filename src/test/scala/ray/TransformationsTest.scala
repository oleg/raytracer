package ray

import org.scalatest.FunSuite
import ray.Matrix._

import scala.math.{Pi, sqrt}

class TransformationsTest extends FunSuite {

  test("Multiplying by a translation matrix") {
    val transform = TranslationMatrix4x4(5, -3, 2)
    val p = Point(-3, 4, 5)

    assert(transform * p ==~ Point(2, 1, 7))
  }

  test("Multiplying by the inverse of a translation matrix") {
    val transform = TranslationMatrix4x4(5, -3, 2)
    val inv = transform.inverse
    val p = Point(-3, 4, 5)

    assert(inv * p ==~ Point(-8, 7, 3))
  }

  test("Translation does not affect vectors") {
    val transform = TranslationMatrix4x4(5, -3, 2)
    val v = Vector(-3, 4, 5)

    assert(transform * v ==~ v)
  }

  test("A scaling matrix applied to a point") {
    val transform = ScalingMatrix4x4(2, 3, 4)
    val p = Point(-4, 6, 8)

    assert(transform * p ==~ Point(-8, 18, 32))
  }

  test("A scaling matrix applied to a vector") {
    val transform = ScalingMatrix4x4(2, 3, 4)
    val v = Vector(-4, 6, 8)

    assert(transform * v ==~ Vector(-8, 18, 32))

  }

  test("Multiplying by the inverse of a scaling matrix") {
    val transform = ScalingMatrix4x4(2, 3, 4)
    val inv = transform.inverse
    val v = Vector(-4, 6, 8)

    assert(inv * v ==~ Vector(-2, 2, 2))
  }

  test("Reflection is scaling by a negative value") {
    val transform = ScalingMatrix4x4(-1, 1, 1)
    val p = Point(2, 3, 4)

    assert(transform * p ==~ Point(-2, 3, 4))
  }

  test("Rotating a point around the x axis") {
    val p = Point(0, 1, 0)

    val halfQuarter = RotationXMatrix4x4(Pi / 4)
    val fullQuarter = RotationXMatrix4x4(Pi / 2)

    assert(halfQuarter * p ==~ Point(0, sqrt(2) / 2, sqrt(2) / 2))
    assert(fullQuarter * p ==~ Point(0, 0, 1))
  }

  test("Rotating another point around the x axis") {
    val p = Point(1, 0, 1)

    val a = RotationXMatrix4x4(Pi / 2)

    assert(a * p ==~ Point(1, -1, 0))
  }

  test("The inverse of an x-rotation rotates in the opposite direction") {
    val p = Point(0, 1, 0)
    val halfQuarter = RotationXMatrix4x4(Pi / 4)
    val inv = halfQuarter.inverse

    assert(inv * p ==~ Point(0, sqrt(2) / 2, -sqrt(2) / 2))
  }

  test("Rotating a point around the y axis") {
    val p = Point(0, 0, 1)

    val halfQuarter = RotationYMatrix4x4(Pi / 4)
    val fullQuarter = RotationYMatrix4x4(Pi / 2)

    assert(halfQuarter * p ==~ Point(sqrt(2) / 2, 0, sqrt(2) / 2))
    assert(fullQuarter * p ==~ Point(1, 0, 0))
  }

  test("Rotating a point around the z axis") {
    val p = Point(0, 1, 0)
    val halfQuarter = RotationZMatrix4x4(Pi / 4)
    val fullQuarter = RotationZMatrix4x4(Pi / 2)

    assert(halfQuarter * p ==~ Point(-sqrt(2) / 2, sqrt(2) / 2, 0))
    assert(fullQuarter * p ==~ Point(-1, 0, 0))
  }

  test("A shearing transformation moves x in proportion to y") {
    val transform = ShearingMatrix4x4(1, 0, 0, 0, 0, 0)
    val p = Point(2, 3, 4)

    assert(transform * p ==~ Point(5, 3, 4))
  }

  test("A shearing transformation moves x in proportion to z") {
    val transform = ShearingMatrix4x4(0, 1, 0, 0, 0, 0)
    val p = Point(2, 3, 4)

    assert(transform * p ==~ Point(6, 3, 4))
  }

  test("A shearing transformation moves y in proportion to x") {
    val transform = ShearingMatrix4x4(0, 0, 1, 0, 0, 0)
    val p = Point(2, 3, 4)

    assert(transform * p ==~ Point(2, 5, 4))
  }

  test("A shearing transformation moves y in proportion to z") {
    val transform = ShearingMatrix4x4(0, 0, 0, 1, 0, 0)
    val p = Point(2, 3, 4)

    assert(transform * p ==~ Point(2, 7, 4))
  }

  test("A shearing transformation moves z in proportion to x") {
    val transform = ShearingMatrix4x4(0, 0, 0, 0, 1, 0)
    val p = Point(2, 3, 4)

    assert(transform * p ==~ Point(2, 3, 6))
  }

  test("A shearing transformation moves z in proportion to y") {
    val transform = ShearingMatrix4x4(0, 0, 0, 0, 0, 1)
    val p = Point(2, 3, 4)

    assert(transform * p ==~ Point(2, 3, 7))
  }

  test("Individual transformations are applied in sequence") {
    val p = Point(1, 0, 1)

    val a = RotationXMatrix4x4(Pi / 2)
    val b = ScalingMatrix4x4(5, 5, 5)
    val c = TranslationMatrix4x4(10, 5, 7)

    val p2 = a * p
    assert(p2 ==~ Point(1, -1, 0))


    val p3 = b * p2
    assert(p3 ==~ Point(5, -5, 0))

    val p4 = c * p3
    assert(p4 ==~ Point(15, 0, 7))
  }

  test("Chained transformations must be applied in reverse order") {
    val p = Point(1, 0, 1)

    val a = RotationXMatrix4x4(Pi / 2)
    val b = ScalingMatrix4x4(5, 5, 5)
    val c = TranslationMatrix4x4(10, 5, 7)

    val t = c * b * a
    assert((t * p) ==~ Point(15, 0, 7))
  }

}
