package ray.tracer
import ray.shapes._
import ray.raymath.RayMath._
import org.scalatest.FunSuite

import scala.math.Pi

class ShapeTest extends FunSuite {

  case class TestShape(transform: Matrix4x4 = Matrix4x4.Identity,
                       material: Material = Material(),
                       parent: Shape = null,
                       var savedRay: Ray = null) extends Shape {

    //todo replace with mocks
    override def localIntersect(localRay: Ray): Intersections = {
      savedRay = localRay
      null
    }

    override def localNormalAt(localPoint: Point, intersection: Intersection): Vector = localPoint- Point(0,0,0)
  }

  test("The default transformation") {
    val s = TestShape()

    assert(s.transform == Matrix4x4.Identity)
  }

  test("Assigning a transformation") {
    val t = Matrix4x4.Translation(2, 3, 4)
    val s = TestShape(transform = t)

    assert(s.transform == t)
  }

  test("The default material") {
    val s = TestShape()

    assert(s.material == Material())
  }

  test("Assigned a material") {
    val m = Material(ambient = 1)
    val s = TestShape(material = m)

    assert(s.material == m)
  }

  test("Intersecting a scaled shape with a ray") {
    val r = Ray(Point(0, 0, -5), Vector(0, 0, 1))
    val s = TestShape(transform = Matrix4x4.Scaling(2, 2, 2))

    s.intersect(r)

    assert(s.savedRay.origin ==~ Point(0, 0, -2.5))
    assert(s.savedRay.direction ==~ Vector(0, 0, 0.5))
  }

  test("Intersecting a translated shape with a ray") {
    val r = Ray(Point(0, 0, -5), Vector(0, 0, 1))
    val s = TestShape(transform = Matrix4x4.Translation(5, 0, 0))

    s.intersect(r)

    assert(s.savedRay.origin ==~ Point(-5, 0, -5))
    assert(s.savedRay.direction ==~ Vector(0, 0, 1))
  }

  test("Computing the normal on a translated shape") {
    val s = TestShape(transform = Matrix4x4.Translation(0, 1, 0))

    val n = s.normalAt(Point(0, 1.70711, -0.70711), null)

    assert(n ==~ Vector(0, 0.70711, -0.70711))
  }

  test("Computing the normal on a transformed shape") {
    val s = TestShape(transform = Matrix4x4.Scaling(1, 0.5, 1) * Matrix4x4.RotationZ(Pi / 5))

    val n = s.normalAt(Point(0, Sqrt2Div2, -Sqrt2Div2), null)

    assert(n ==~ Vector(0, 0.97014, -0.24254))
  }
}
