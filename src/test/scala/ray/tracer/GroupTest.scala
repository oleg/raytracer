package ray.tracer

import org.scalatest.funsuite.AnyFunSuite
import ray.tracer.shape.ShapeFactory._
import ray.tracer.shape.{Group, Shape}

import scala.math.Pi

class GroupTest extends AnyFunSuite {

  case class TestShape(transform: Matrix4x4 = Matrix4x4.Identity,
                       material: Material = Material(),
                       var savedRay: Ray = null) extends Shape {

    //todo replace with mocks
    override def localIntersect(localRay: Ray): Intersections = {
      savedRay = localRay
      null
    }

    override def localNormalAt(localPoint: Point, intersection: Intersection): Vector = localPoint - Point(0, 0, 0)
  }


  //TODO:Oleg do we need size test?
  //  test("Creating a new group") {
  //    val g = Group()
  //
  //    assert(g.transform == Matrix4x4.Identity)
  //    assert(g.size == 0)
  //  }

  //TODO:Oleg do we need contains test?
  //  test("Adding a child to a group") {
  //    val g = Group()
  //    val s = TestShape()
  //
  //    g.add(s)
  //
  //    assert(g.size == 1)
  //    assert(g.contains(s))
  //    assert(s.parent == g)
  //  }

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

  test("Intersecting a transformed group tracks transforms") {
    val groupTs = Matrix4x4.Scaling(2, 2, 2)
    val shapeTs = Matrix4x4.Translation(5, 0, 0)

    val g = Group(transform = groupTs)
    val s = Sphere(transform = shapeTs)

    g.add(s)

    val r = Ray(Point(10, 0, -10), Vector(0, 0, 1))

    val xs = g.intersect(r).toList

    assert(xs.head.ts == groupTs :: shapeTs :: Nil)
    assert(xs.tail.head.ts == groupTs :: shapeTs :: Nil)
  }

  test("Converting a point from world to object space") {
    val g1 = Group(transform = Matrix4x4.RotationY(Pi / 2.0))
    val g2 = Group(transform = Matrix4x4.Scaling(2, 2, 2))
    val s = Sphere(transform = Matrix4x4.Translation(5, 0, 0))

    g1.add(g2)
    g2.add(s)

    val p = Intersection(Double.NaN, s, g1.transform :: g2.transform :: s.transform :: Nil).worldToObject(Point(-2, 0, -10))

    assert(p ==~ Point(0, 0, -1))
  }

  test("Converting a normal from object to world space") {
    val g1 = Group(transform = Matrix4x4.RotationY(Pi / 2))
    val g2 = Group(transform = Matrix4x4.Scaling(1, 2, 3))
    val s = Sphere(transform = Matrix4x4.Translation(5, 0, 0))

    g2.add(s)
    g1.add(g2)

    val normal = Vector(math.sqrt(3) / 3.0, math.sqrt(3) / 3.0, math.sqrt(3) / 3.0)

    val n = Intersection(Double.NaN, s, g1.transform :: g2.transform :: s.transform :: Nil).normalToWorld(normal)

    assert(n ==~ Vector(0.28571, 0.42857, -0.85714), n)
  }

  test("Finding the normal on a child object") {
    val g1 = Group(transform = Matrix4x4.RotationY(Pi / 2))
    val g2 = Group(transform = Matrix4x4.Scaling(1, 2, 3))
    val s = Sphere(transform = Matrix4x4.Translation(5, 0, 0))

    g1.add(g2)
    g2.add(s)

    val p = Point(1.7321, 1.1547, -5.5774)
    //    val n = s.normalAt(, null)
    val n = Intersection(Double.NaN, s, g1.transform :: g2.transform :: s.transform :: Nil).myNormalAt(s, p)

    assert(n ==~ Vector(0.28571, 0.42854, -0.85716), n)
  }
}
