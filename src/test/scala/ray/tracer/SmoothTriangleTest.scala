package ray.tracer

import org.scalatest.funsuite.AnyFunSuite
import ray.tracer.shape.ShapeFactory._
import ray.tracer.shape.{Shape, SimpleShape}
import ray.tracer.shapemath.SmoothTriangleMath

class SmoothTriangleTest extends AnyFunSuite {
  val p = implicitly[Precision[Double]]
  test("Constructing a smooth triangle") {
    val tri = smoothTriangle().asInstanceOf[SimpleShape].math.asInstanceOf[SmoothTriangleMath]
    assert(tri.p1 == Point(0, 1, 0))
    assert(tri.p2 == Point(-1, 0, 0))
    assert(tri.p3 == Point(1, 0, 0))
    assert(tri.n1 == Vector(0, 1, 0))
    assert(tri.n2 == Vector(-1, 0, 0))
    assert(tri.n3 == Vector(1, 0, 0))
  }

  test("An intersection can encapsulate `u` and `v`") {
    val s = Triangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0))


    val i = Intersection(3.5, s, 0.2, 0.4)
    assert(i.u == 0.2)
    assert(i.v == 0.4)
  }

  test("An intersection with a smooth triangle stores u/v") {
    val tri = smoothTriangle()
    val r = Ray(Point(-0.2, 0.3, -2), Vector(0, 0, 1))
    val xs = tri.localIntersect(r)

    assert(p.approximatelyEqual(xs(0).u, 0.45))
    assert(p.approximatelyEqual(xs(0).v, 0.25))
  }

  test("A smooth triangle uses u/v to interpolate the normal") {
    val tri = smoothTriangle()
    val i = Intersection(1, tri, 0.45, 0.25)

    val n = tri.normalAt(Point(0, 0, 0), i)

    assert(n ==~ Vector(-0.5547, 0.83205, 0))
  }

  test("Preparing the normal on a smooth triangle") {
    val tri = smoothTriangle()
    val r = Ray(Point(-0.2, 0.3, -2), Vector(0, 0, 1))
    val i = Intersection(1, tri, 0.45, 0.25)
    val xs = Intersections(i :: Nil)

    val comps = i.prepareComputations(r, xs.findNs(i))

    assert(comps.normalv ==~ Vector(-0.5547, 0.83205, 0))
  }


  private def smoothTriangle(): Shape = {
    SmoothTriangle(
      Point(0, 1, 0),
      Point(-1, 0, 0),
      Point(1, 0, 0),
      Vector(0, 1, 0),
      Vector(-1, 0, 0),
      Vector(1, 0, 0))
  }
}
