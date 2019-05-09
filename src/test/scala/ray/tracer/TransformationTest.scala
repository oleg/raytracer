package ray.tracer

import org.scalatest.FunSuite

import scala.math.{Pi, sqrt}

class TransformationTest extends FunSuite {

  test("Multiplying by a translation matrix") {
    val transform = Matrix4x4.Translation(5, -3, 2)
    val p = Point(-3, 4, 5)

    assert(transform * p ==~ Point(2, 1, 7))
  }

  test("Multiplying by the inverse of a translation matrix") {
    val transform = Matrix4x4.Translation(5, -3, 2)
    val inv = transform.inverse
    val p = Point(-3, 4, 5)

    assert(inv * p ==~ Point(-8, 7, 3))
  }

  test("Translation does not affect vectors") {
    val transform = Matrix4x4.Translation(5, -3, 2)
    val v = Vector(-3, 4, 5)

    assert(transform * v ==~ v)
  }

  test("A scaling matrix applied to a point") {
    val transform = Matrix4x4.Scaling(2, 3, 4)
    val p = Point(-4, 6, 8)

    assert(transform * p ==~ Point(-8, 18, 32))
  }

  test("A scaling matrix applied to a vector") {
    val transform = Matrix4x4.Scaling(2, 3, 4)
    val v = Vector(-4, 6, 8)

    assert(transform * v ==~ Vector(-8, 18, 32))

  }

  test("Multiplying by the inverse of a scaling matrix") {
    val transform = Matrix4x4.Scaling(2, 3, 4)
    val inv = transform.inverse
    val v = Vector(-4, 6, 8)

    assert(inv * v ==~ Vector(-2, 2, 2))
  }

  test("Reflection is scaling by a negative value") {
    val transform = Matrix4x4.Scaling(-1, 1, 1)
    val p = Point(2, 3, 4)

    assert(transform * p ==~ Point(-2, 3, 4))
  }

  test("Rotating a point around the x axis") {
    val p = Point(0, 1, 0)

    val halfQuarter = Matrix4x4.RotationX(Pi / 4)
    val fullQuarter = Matrix4x4.RotationX(Pi / 2)

    assert(halfQuarter * p ==~ Point(0, Sqrt2Div2, Sqrt2Div2))
    assert(fullQuarter * p ==~ Point(0, 0, 1))
  }

  test("Rotating another point around the x axis") {
    val p = Point(1, 0, 1)

    val a = Matrix4x4.RotationX(Pi / 2)

    assert(a * p ==~ Point(1, -1, 0))
  }

  test("The inverse of an x-rotation rotates in the opposite direction") {
    val p = Point(0, 1, 0)
    val halfQuarter = Matrix4x4.RotationX(Pi / 4)
    val inv = halfQuarter.inverse

    assert(inv * p ==~ Point(0, Sqrt2Div2, -Sqrt2Div2))
  }

  test("Rotating a point around the y axis") {
    val p = Point(0, 0, 1)

    val halfQuarter = Matrix4x4.RotationY(Pi / 4)
    val fullQuarter = Matrix4x4.RotationY(Pi / 2)

    assert(halfQuarter * p ==~ Point(Sqrt2Div2, 0, Sqrt2Div2))
    assert(fullQuarter * p ==~ Point(1, 0, 0))
  }

  test("Rotating a point around the z axis") {
    val p = Point(0, 1, 0)
    val halfQuarter = Matrix4x4.RotationZ(Pi / 4)
    val fullQuarter = Matrix4x4.RotationZ(Pi / 2)

    assert(halfQuarter * p ==~ Point(-Sqrt2Div2, Sqrt2Div2, 0))
    assert(fullQuarter * p ==~ Point(-1, 0, 0))
  }

  test("A shearing transformation moves x in proportion to y") {
    val transform = Matrix4x4.Shearing(1, 0, 0, 0, 0, 0)
    val p = Point(2, 3, 4)

    assert(transform * p ==~ Point(5, 3, 4))
  }

  test("A shearing transformation moves x in proportion to z") {
    val transform = Matrix4x4.Shearing(0, 1, 0, 0, 0, 0)
    val p = Point(2, 3, 4)

    assert(transform * p ==~ Point(6, 3, 4))
  }

  test("A shearing transformation moves y in proportion to x") {
    val transform = Matrix4x4.Shearing(0, 0, 1, 0, 0, 0)
    val p = Point(2, 3, 4)

    assert(transform * p ==~ Point(2, 5, 4))
  }

  test("A shearing transformation moves y in proportion to z") {
    val transform = Matrix4x4.Shearing(0, 0, 0, 1, 0, 0)
    val p = Point(2, 3, 4)

    assert(transform * p ==~ Point(2, 7, 4))
  }

  test("A shearing transformation moves z in proportion to x") {
    val transform = Matrix4x4.Shearing(0, 0, 0, 0, 1, 0)
    val p = Point(2, 3, 4)

    assert(transform * p ==~ Point(2, 3, 6))
  }

  test("A shearing transformation moves z in proportion to y") {
    val transform = Matrix4x4.Shearing(0, 0, 0, 0, 0, 1)
    val p = Point(2, 3, 4)

    assert(transform * p ==~ Point(2, 3, 7))
  }

  test("Individual transformations are applied in sequence") {
    val p = Point(1, 0, 1)

    val a = Matrix4x4.RotationX(Pi / 2)
    val b = Matrix4x4.Scaling(5, 5, 5)
    val c = Matrix4x4.Translation(10, 5, 7)

    val p2 = a * p
    assert(p2 ==~ Point(1, -1, 0))


    val p3 = b * p2
    assert(p3 ==~ Point(5, -5, 0))

    val p4 = c * p3
    assert(p4 ==~ Point(15, 0, 7))
  }

  test("Chained transformations must be applied in reverse order") {
    val p = Point(1, 0, 1)

    val a = Matrix4x4.RotationX(Pi / 2)
    val b = Matrix4x4.Scaling(5, 5, 5)
    val c = Matrix4x4.Translation(10, 5, 7)

    val t = c * b * a
    assert((t * p) ==~ Point(15, 0, 7))
  }

  test("Chained transformations with fluent API") {
    val p = Point(1, 0, 1)

    val t = Matrix4x4.Identity
      .rotateX(Pi / 2)
      .scale(5, 5, 5)
      .translate(10, 5, 7)

    assert((t * p) ==~ Point(15, 0, 7))
  }

  test("The transformation matrix for the default orientation") {
    val from = Point(0, 0, 0)
    val to = Point(0, 0, -1)
    val up = Vector(0, 1, 0)

    val t = Matrix4x4.viewTransform(from, to, up)

    assert(t ==~ Matrix4x4.Identity)
  }

  test("A view transformation matrix looking in positive z direction") {
    val from = Point(0, 0, 0)
    val to = Point(0, 0, 1)
    val up = Vector(0, 1, 0)

    val t = Matrix4x4.viewTransform(from, to, up)

    assert(t ==~ Matrix4x4.Scaling(-1, 1, -1))
  }

  test("The view transformation moves the world") {
    val from = Point(0, 0, 8)
    val to = Point(0, 0, 0)
    val up = Vector(0, 1, 0)

    val t = Matrix4x4.viewTransform(from, to, up)

    assert(t ==~ Matrix4x4.Translation(0, 0, -8), "\n\n" + t)
  }

  test("An arbitrary view transformation") {
    val from = Point(1, 3, 2)
    val to = Point(4, -2, 8)
    val up = Vector(1, 1, 0)

    val t = Matrix4x4.viewTransform(from, to, up)

    assert(t ==~ Matrix4x4(DoubleArray(
      """
        | -0.50709 | 0.50709 |  0.67612 | -2.36643 |
        |  0.76772 | 0.60609 |  0.12122 | -2.82843 |
        | -0.35857 | 0.59761 | -0.71714 |  0.00000 |
        |  0.00000 | 0.00000 |  0.00000 |  1.00000 |
      """)), "\n\n" + t)
  }

}
