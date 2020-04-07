package ray.tracer

import org.scalatest.funsuite.AnyFunSuite
import ray.tracer.shape.ShapeFactory._
import ray.tracer.shapemath.ConeMath

class ConeTest extends AnyFunSuite {
  val p = implicitly[Precision[Double]]
  test("Intersecting a cone with a ray, 5;5") {
    val ray = Ray(Point(0, 0, -5), Vector(0, 0, 1).normalize)

    val xs = Cone().localIntersect(ray)

    assert(xs.length == 2)
    assert(xs(0).t == 5)
    assert(xs(1).t == 5)
  }

  test("Intersecting a cone with a ray, 8,6;8,6") {
    val ray = Ray(Point(0, 0, -5), Vector(1, 1, 1).normalize)

    val xs = Cone().localIntersect(ray)

    assert(xs.length == 2)
    assert(p.approximatelyEqual(xs(0).t, 8.66025))
    assert(p.approximatelyEqual(xs(1).t, 8.66025))
  }

  test("Intersecting a cone with a ray, 4;49") {
    val ray = Ray(Point(1, 1, -5), Vector(-0.5, -1, 1).normalize)

    val xs = Cone().localIntersect(ray)

    assert(xs.length == 2)
    assert(p.approximatelyEqual(xs(0).t, 4.55006))
    assert(p.approximatelyEqual(xs(1).t, 49.44994))
  }

  test("Intersecting a cone with a ray parallel to one of its halves") {
    val ray = Ray(Point(0, 0, -1), Vector(0, 1, 1).normalize)

    val xs = Cone().localIntersect(ray)
    assert(xs.length == 1)
    assert(p.approximatelyEqual(xs(0).t, 0.35355))
  }

  test("Intersecting a cone's end caps; -5") {
    val cone = Cone(minimum = -0.5, maximum = 0.5, closed = true)
    val ray = Ray(Point(0, 0, -5), Vector(0, 1, 0).normalize)

    assert(cone.localIntersect(ray).length == 0)
  }

  test("Intersecting a cone's end caps; -0.25, 1") {
    val cone = Cone(minimum = -0.5, maximum = 0.5, closed = true)
    val ray = Ray(Point(0, 0, -0.25), Vector(0, 1, 1).normalize)

    assert(cone.localIntersect(ray).length == 2)
  }

  test("Intersecting a cone's end caps; -0.25, 0") {
    val cone = Cone(minimum = -0.5, maximum = 0.5, closed = true)
    val ray = Ray(Point(0, 0, -0.25), Vector(0, 1, 0).normalize)

    assert(cone.localIntersect(ray).length == 4)
  }

  test("Computing the normal vector on a cone, 0") {
    assert(ConeMath().normalAt(Point(0, 0, 0), null) == Vector(0, 0, 0))
  }

  test("Computing the normal vector on a cone, 1") {
    assert(ConeMath().normalAt(Point(1, 1, 1), null) == Vector(1, -Sqrt2, 1))
  }

  test("Computing the normal vector on a cone, -1") {
    assert(ConeMath().normalAt(Point(-1, -1, 0), null) == Vector(-1, 1, 0))
  }
}