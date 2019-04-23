package ray

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

    val xs = Intersections(i1, i2)
    assert(xs.count == 2)
    assert(xs(0).t == 1)
    assert(xs(1).t == 2)
  }

  test("The hit, when all intersections have positive t") {
    val sphere = Sphere()
    val i1 = Intersection(1, sphere)
    val i2 = Intersection(2, sphere)

    val xs = Intersections(i2, i1)
    assert(xs.hit.contains(i1))
  }

  test("The hit, when some intersections have negative t") {
    val sphere = Sphere()
    val i1 = Intersection(-1, sphere)
    val i2 = Intersection(1, sphere)

    val xs = Intersections(i2, i1)
    assert(xs.hit.contains(i2))
  }

  test("The hit, when all intersections have negative t") {
    val sphere = Sphere()
    val i1 = Intersection(-2, sphere)
    val i2 = Intersection(-1, sphere)

    val xs = Intersections(i2, i1)
    assert(xs.hit.isEmpty)
  }
  test("The hit is always the lowest nonnegative intersection") {
    val sphere = Sphere()

    val i1 = Intersection(5, sphere)
    val i2 = Intersection(7, sphere)
    val i3 = Intersection(-3, sphere)
    val i4 = Intersection(2, sphere)

    val xs = Intersections(i1, i2, i3, i4)

    assert(xs.hit.contains(i4))
  }

}
