package ray.tracer

import org.scalatest.FunSuite

class GroupTest extends FunSuite {

  case class TestShape(transform: Matrix4x4 = Matrix4x4.Identity,
                       material: Material = Material(),
                       var parent: Group = null,
                       var savedRay: Ray = null) extends Shape {

    //todo replace with mocks
    override def localIntersect(localRay: Ray): Intersections = {
      savedRay = localRay
      null
    }

    override def localNormalAt(localPoint: Tuple): Tuple = localPoint.toVector
  }


  test("Creating a new group") {
    val g = Group()

    assert(g.transform == Matrix4x4.Identity)
    assert(g.size == 0)
  }

  test("A shape has a parent attribute") {
    val s = TestShape()
    assert(s.parent == null)
  }

  test("Adding a child to a group") {
    val g = Group()
    val s = TestShape()

    g.add(s)

    assert(g.size == 1)
    assert(g.contains(s))
    assert(s.parent == g)
  }

  test("Intersecting a ray with an empty group") {
    val g = Group()
    val r = Ray(Point(0, 0, 0), Vector(0, 0, 1))

    val xs = g.localIntersect(r)

    assert(xs.isEmpty)
  }

  test("Intersecting a ray with a nonempty group") {
    val g = Group()
    val s1 = Sphere()
    val s2 = Sphere(transform = Matrix4x4.Translation(0, 0, -3))
    val s3 = Sphere(transform = Matrix4x4.Translation(5, 0, 0))

    g.add(s1)
    g.add(s2)
    g.add(s3)

    val r = Ray(Point(0, 0, -5), Vector(0, 0, 1))

    val xs = g.localIntersect(r)

    assert(xs.length == 4)
    assert(xs(0).obj == s2)
    assert(xs(1).obj == s2)
    assert(xs(2).obj == s1)
    assert(xs(3).obj == s1)
  }

  test("Intersecting a transformed group") {
    val g = Group(transform = Matrix4x4.Scaling(2, 2, 2))
    val s = Sphere(transform = Matrix4x4.Translation(5, 0, 0))

    g.add(s)

    val r = Ray(Point(10, 0, -10), Vector(0, 0, 1))

    val xs = g.intersect(r)

    assert(xs.length == 2)
  }

}
