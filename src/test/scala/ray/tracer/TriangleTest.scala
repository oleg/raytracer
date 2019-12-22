package ray.tracer

import org.scalatest.FunSuite
import ray.tracer.shapemath.TriangleMath

class TriangleTest extends FunSuite {

  test("Constructing a triangle") {
    val p1 = Point(0, 1, 0)
    val p2 = Point(-1, 0, 0)
    val p3 = Point(1, 0, 0)

    val t = TriangleMath(p1, p2, p3)

    assert(t.p1 == p1)
    assert(t.p2 == p2)
    assert(t.p3 == p3)

    assert(t.e1 == Vector(-1, -1, 0))
    assert(t.e2 == Vector(1, -1, 0))
    assert(t.normal == Vector(0, 0, -1))
  }

  test("Finding the normal on a triangle") {
    val t = Triangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0))
    val tm = TriangleMath(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0))

    assert(t.localNormalAt(Point(0, 0.5, 0),null) == tm.normal)
    assert(t.localNormalAt(Point(-0.5, 0.75, 0),null) == tm.normal)
    assert(t.localNormalAt(Point(0.5, 0.25, 0),null) == tm.normal)
  }

  test("Intersecting a ray parallel to the triangle") {
    val t = Triangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0))
    val r = Ray(Point(0, -1, -2), Vector(0, 1, 0))

    val xs = t.localIntersect(r)

    assert(xs.isEmpty)
  }

  test("A ray misses the p1-p3 edge") {
    val t = Triangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0))
    val r = Ray(Point(1, 1, -2), Vector(0, 0, 1))

    val xs = t.localIntersect(r)

    assert(xs.isEmpty)
  }

  test("A ray misses the p1-p2 edge") {
    val t = Triangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0))
    val r = Ray(Point(-1, 1, -2), Vector(0, 0, 1))

    val xs = t.localIntersect(r)

    assert(xs.isEmpty)
  }

  test("A ray misses the p2-p3 edge") {
    val t = Triangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0))
    val r = Ray(Point(0, -1, -2), Vector(0, 0, 1))

    val xs = t.localIntersect(r)

    assert(xs.isEmpty)
  }

  test("A ray strikes a triangle") {
    val t = Triangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0))
    val r = Ray(Point(0, 0.5, -2), Vector(0, 0, 1))

    val xs = t.localIntersect(r)

    assert(xs.length == 1)
    assert(xs(0).t == 2)
  }
}
