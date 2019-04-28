package ray.tracer

import org.scalatest.FunSuite

class IntersectionTest extends FunSuite {

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
    val comps = i.prepareComputations(r)

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

    val comps = i.prepareComputations(r)

    assert(comps.inside == false)
  }

  test("The hit, when an intersection occurs on the inside") {
    val r = Ray(Point(0, 0, 0), Vector(0, 0, 1))
    val shape = Sphere()
    val i = Intersection(1, shape)

    val comps = i.prepareComputations(r)
    assert(comps.point == Point(0, 0, 1))
    assert(comps.eyev == Vector(0, 0, -1))
    assert(comps.normalv == Vector(0, 0, -1))
    assert(comps.inside == true)

  }
}
