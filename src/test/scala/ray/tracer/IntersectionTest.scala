package ray.tracer

import org.scalatest.funsuite.AnyFunSuite
import ray.tracer.shape.ShapeFactory._

class IntersectionTest extends AnyFunSuite {
  private val p: Precision[Double] = implicitly[Precision[Double]]
  test("An intersection encapsulates t and object") {
    val ray = Ray(Point(0, 0, -5), Vector(0, 0, 1))
    val sphere = Sphere()

    val data = Intersection(3.5, sphere)
    assert(data.t == 3.5)
    assert(data.obj == sphere)
  }

  test("Aggregating intersections") {
    val sphere = Sphere()
    val i1 = Intersection(1, sphere)
    val i2 = Intersection(2, sphere)

    val xs = Intersections(i1 :: i2 :: Nil)
    assert(xs.length == 2)
    assert(xs(0).t == 1)
    assert(xs(1).t == 2)
  }

  test("The hit, when all intersections have positive t") {
    val sphere = Sphere()
    val i1 = Intersection(1, sphere)
    val i2 = Intersection(2, sphere)

    val xs = Intersections(i2 :: i1 :: Nil)
    assert(xs.hit.contains(i1))
  }

  test("The hit, when some intersections have negative t") {
    val sphere = Sphere()
    val i1 = Intersection(-1, sphere)
    val i2 = Intersection(1, sphere)

    val xs = Intersections(i2 :: i1 :: Nil)
    assert(xs.hit.contains(i2))
  }

  test("The hit, when all intersections have negative t") {
    val sphere = Sphere()
    val i1 = Intersection(-2, sphere)
    val i2 = Intersection(-1, sphere)

    val xs = Intersections(i2 :: i1 :: Nil)
    assert(xs.hit.isEmpty)
  }

  test("The hit is always the lowest nonnegative intersection") {
    val sphere = Sphere()

    val i1 = Intersection(5, sphere)
    val i2 = Intersection(7, sphere)
    val i3 = Intersection(-3, sphere)
    val i4 = Intersection(2, sphere)

    val xs = Intersections(i1 :: i2 :: i3 :: i4 :: Nil)

    assert(xs.hit.contains(i4))
  }

  test("Precomputing the state of an intersection") {
    val r = Ray(Point(0, 0, -5), Vector(0, 0, 1))
    val shape = Sphere()
    val i = Intersection(4, shape)
    val ns = Intersections(i :: Nil).findNs(i) //todo:ns
    val comps = i.prepareComputations(r, ns)

    assert(comps.t == i.t)
    assert(comps.obj == i.obj)
    assert(comps.point == Point(0, 0, -1))
    assert(comps.eyev == Vector(0, 0, -1))
    assert(comps.normalv == Vector(0, 0, -1))
  }

  test("The hit, when an intersection occurs on the outside") {
    val r = Ray(Point(0, 0, -5), Vector(0, 0, 1))
    val shape = Sphere()
    val i = Intersection(4, shape)
    val ns = Intersections(i :: Nil).findNs(i)
    val comps = i.prepareComputations(r, ns)

    assert(comps.inside == false)
  }

  test("The hit, when an intersection occurs on the inside") {
    val r = Ray(Point(0, 0, 0), Vector(0, 0, 1))
    val shape = Sphere()
    val i = Intersection(1, shape)
    val ns = Intersections(i :: Nil).findNs(i)
    val comps = i.prepareComputations(r, ns)
    assert(comps.point == Point(0, 0, 1))
    assert(comps.eyev == Vector(0, 0, -1))
    assert(comps.normalv == Vector(0, 0, -1))
    assert(comps.inside == true)
  }

  test("The hit should offset the point") {
    val r = Ray(Point(0, 0, -5), Vector(0, 0, 1))
    val shape = Sphere(Matrix4x4.Identity.translate(0, 0, 1))
    val i = Intersection(5, shape)
    val ns = Intersections(i :: Nil).findNs(i)
    val comps = i.prepareComputations(r, ns)

    assert(comps.overPoint.z < (-p.precision / 2))
    assert(comps.point.z > comps.overPoint.z)
  }

  test("Precomputing the reflection vector") {
    val shape = Plane()
    val r = Ray(Point(0, 1, -1), Vector(0, -Sqrt2Div2, Sqrt2Div2))
    val i = Intersection(Sqrt2, shape)
    val ns = Intersections(i :: Nil).findNs(i)
    val comps = i.prepareComputations(r, ns)

    assert(comps.reflectv == Vector(0, Sqrt2Div2, Sqrt2Div2))
  }

  test("The under point is offset below the surface") {
    val r = Ray(Point(0, 0, -5), Vector(0, 0, 1))
    val shape = glassSphere(transform = Matrix4x4.Translation(0, 0, 1))
    val i = Intersection(5, shape)

    val xs = Intersections(i :: Nil)
    val ns = xs.findNs(i)

    val comps = i.prepareComputations(r, ns)

    assert(comps.underPoint.z > p.precision / 2)
    assert(comps.point.z < comps.underPoint.z)
  }

  test("The Schlick approximation under total internal reflection") {
    val shape = glassSphere()
    val r = Ray(Point(0, 0, Sqrt2Div2), Vector(0, 1, 0))
    val xs = Intersections(Intersection(-Sqrt2Div2, shape) :: Intersection(Sqrt2Div2, shape) :: Nil)

    val ns = xs.findNs(xs(1))
    val comps = xs(1).prepareComputations(r, ns)
    val reflectance = comps.schlick()

    assert(reflectance == 1.0)
  }

  test("The Schlick approximation with a perpendicular viewing angle") {
    val shape = glassSphere()
    val r = Ray(Point(0, 0, 0), Vector(0, 1, 0))
    val xs = Intersections(Intersection(-1, shape) :: Intersection(1, shape) :: Nil)

    val ns = xs.findNs(xs(1))
    val comps = xs(1).prepareComputations(r, ns)
    val reflectance = comps.schlick()

    assert(p.approximatelyEqual(reflectance, 0.04), reflectance)
  }

  test("The Schlick approximation with small angle and n2 > n1") {
    val shape = glassSphere()
    val r = Ray(Point(0, 0.99, -2), Vector(0, 0, 1))
    val xs = Intersections(Intersection(1.8589, shape) :: Nil)

    val ns = xs.findNs(xs(0))
    val comps = xs(0).prepareComputations(r, ns)
    val reflectance = comps.schlick()

    assert(p.approximatelyEqual(reflectance, 0.48873), reflectance)
  }
}
