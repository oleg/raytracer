package ray.tracer

import org.scalatest.funsuite.AnyFunSuite
import ray.tracer.shape.ShapeFactory._
import ray.tracer.shapemath.PlaneMath

class PlaneTest extends AnyFunSuite {

  test("The normal of a plane is constant everywhere") {
    val p = PlaneMath()

    val n1 = p.normalAt(Point(0, 0, 0), null)
    val n2 = p.normalAt(Point(10, 0, -10), null)
    val n3 = p.normalAt(Point(-5, 0, 150), null)

    assert(n1 == Vector(0, 1, 0))
    assert(n2 == Vector(0, 1, 0))
    assert(n3 == Vector(0, 1, 0))
  }

  test("Intersect with a ray parallel to the plane") {
    val p = Plane()
    val r = Ray(Point(0, 10, 0), Vector(0, 0, 1))

    val xs = p.localIntersect(r)

    assert(xs.length == 0)
  }

  test("Intersect with a coplanar ray") {
    val p = Plane()
    val r = Ray(Point(0, 0, 0), Vector(0, 0, 1))

    val xs = p.localIntersect(r)

    assert(xs.length == 0)
  }

  test("A ray intersecting a plane from above") {
    val p = Plane()
    val r = Ray(Point(0, 1, 0), Vector(0, -1, 1))

    val xs = p.localIntersect(r)

    assert(xs.length == 1)
    assert(xs(0).t == 1)
    assert(xs(0).obj == p)
  }

  test("A ray intersecting a plane from below") {
    val p = Plane()
    val r = Ray(Point(0, -1, 0), Vector(0, 1, 1))

    val xs = p.localIntersect(r)

    assert(xs.length == 1)
    assert(xs(0).t == 1)
    assert(xs(0).obj == p)
  }
}
