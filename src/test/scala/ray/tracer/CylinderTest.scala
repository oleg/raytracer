package ray.tracer

import org.scalatest.FunSuite

class CylinderTest extends FunSuite {

  test("A ray misses a cylinder at origin(1,0,0) with direction(0,1,0)") {
    val origin = Point(1, 0, 0)
    val direction = Vector(0, 1, 0).normalize
    val ray = Ray(origin, direction)

    assert(Cylinder().localIntersect(ray).length == 0)
  }

  test("A ray misses a cylinder at origin(0,0,0) with direction(0,1,0)") {
    val origin = Point(1, 0, 0)
    val direction = Vector(0, 1, 0).normalize
    val ray = Ray(origin, direction)

    assert(Cylinder().localIntersect(ray).length == 0)
  }

  test("A ray misses a cylinder at origin(0,0,-5) with direction(1,1,1)") {
    val origin = Point(1, 0, 0)
    val direction = Vector(0, 1, 0).normalize
    val ray = Ray(origin, direction)

    assert(Cylinder().localIntersect(ray).length == 0)
  }

  test("A ray strikes a cylinder on a tangent") {
    val origin = Point(1, 0, -5)
    val direction = Vector(0, 0, 1).normalize
    val ray = Ray(origin, direction)

    val ixs = Cylinder().localIntersect(ray)
    assert(ixs.length == 2)
    assert(ixs(0).t == 5)
    assert(ixs(1).t == 5)
  }

  test("A ray intersects a cylinder perpendicularly through the middle") {
    val origin = Point(0, 0, -5)
    val direction = Vector(0, 0, 1).normalize
    val ray = Ray(origin, direction)

    val ixs = Cylinder().localIntersect(ray)
    assert(ixs.length == 2)
    assert(ixs(0).t == 4)
    assert(ixs(1).t == 6)
  }

  test("A ray strikes a cylinder at an angle") {
    val origin = Point(0.5, 0, -5)
    val direction = Vector(0.1, 1, 1).normalize
    val ray = Ray(origin, direction)

    val ixs = Cylinder().localIntersect(ray)
    assert(ixs.length == 2)
    assert(approximatelyEqual(ixs(0).t, 6.80798), ixs(0).t)
    assert(approximatelyEqual(ixs(1).t, 7.08872), ixs(1).t)
  }

  test("Normal vector on a cylinder at +x") {
    assert(Cylinder().localNormalAt(Point(1, 0, 0)) == Vector(1, 0, 0))
  }

  test("Normal vector on a cylinder at -x") {
    assert(Cylinder().localNormalAt(Point(0, 5, -1)) == Vector(0, 0, -1))
  }

  test("Normal vector on a cylinder at +z") {
    assert(Cylinder().localNormalAt(Point(0, -2, 1)) == Vector(0, 0, 1))
  }

  test("Normal vector on a cylinder at -z") {
    assert(Cylinder().localNormalAt(Point(-1, 1, 0)) == Vector(-1, 0, 0))
  }

  test("The default minimum and maximum for a cylinder") {
    val cylinder = Cylinder()

    assert(cylinder.minimum == Double.NegativeInfinity)
    assert(cylinder.maximum == Double.PositiveInfinity)
  }

  test("Intersecting a constrained cylinder diagonally from inside the cylinder") {
    val cylinder = Cylinder(minimum = 1, maximum = 2)
    val ray = Ray(Point(0, 1.5, 0), Vector(0.1, 1, 0).normalize)

    assert(cylinder.localIntersect(ray).length == 0)
  }

  test("Intersecting a constrained cylinder perpendicularly to the y axis above the cylinder") {
    val cylinder = Cylinder(minimum = 1, maximum = 2)
    val ray = Ray(Point(0, 3, -5), Vector(0, 0, 1).normalize)

    assert(cylinder.localIntersect(ray).length == 0)
  }

  test("Intersecting a constrained cylinder perpendicularly to the y axis below the cylinder") {
    val cylinder = Cylinder(minimum = 1, maximum = 2)
    val ray = Ray(Point(0, 0, -5), Vector(0, 0, 1).normalize)

    assert(cylinder.localIntersect(ray).length == 0)
  }

  test("Intersecting a constrained cylinder at minimum") {
    val cylinder = Cylinder(minimum = 1, maximum = 2)
    val ray = Ray(Point(0, 2, -5), Vector(0, 0, 1).normalize)

    assert(cylinder.localIntersect(ray).length == 0)
  }

  test("Intersecting a constrained cylinder at maximum") {
    val cylinder = Cylinder(minimum = 1, maximum = 2)
    val ray = Ray(Point(0, 1, -5), Vector(0, 0, 1).normalize)

    assert(cylinder.localIntersect(ray).length == 0)
  }

  test("Intersecting a constrained cylinder perpendicularly through the middle of the cylinder") {
    val cylinder = Cylinder(minimum = 1, maximum = 2)
    val ray = Ray(Point(0, 1.5, -2), Vector(0, 0, 1).normalize)

    assert(cylinder.localIntersect(ray).length == 2)
  }
}
