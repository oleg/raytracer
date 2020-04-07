package ray.tracer

import org.scalatest.funsuite.AnyFunSuite
import ray.tracer.shape.SimpleShape
import ray.tracer.shapemath.{Inter, ShapeMath}

import scala.math.Pi

class ShapeTest extends AnyFunSuite {

  case class TestMath(var savedRay: Ray = null) extends ShapeMath {

    override def intersect(ray: Ray): List[Inter] = {
      savedRay = ray
      Nil
    }

    override def normalAt(point: Point, inter: Inter): Vector =
      point - Point(0, 0, 0)
  }


  test("The default transformation") {
    val s = SimpleShape(math = TestMath())

    assert(s.transform == Matrix4x4.Identity)
  }

  test("Assigning a transformation") {
    val t = Matrix4x4.Translation(2, 3, 4)
    val s = SimpleShape(math = TestMath(), transform = t)

    assert(s.transform == t)
  }

  test("The default material") {
    val s = SimpleShape(math = TestMath())

    assert(s.material == Material())
  }

  test("Assigned a material") {
    val m = Material(ambient = 1)
    val s = SimpleShape(math = TestMath(), material = m)

    assert(s.material == m)
  }

  test("Intersecting a scaled shape with a ray") {
    val r = Ray(Point(0, 0, -5), Vector(0, 0, 1))
    val testMath = TestMath()
    val s = SimpleShape(math = testMath, transform = Matrix4x4.Scaling(2, 2, 2))

    s.intersect(r)

    assert(testMath.savedRay.origin ==~ Point(0, 0, -2.5))
    assert(testMath.savedRay.direction ==~ Vector(0, 0, 0.5))
  }

  test("Intersecting a translated shape with a ray") {
    val r = Ray(Point(0, 0, -5), Vector(0, 0, 1))
    val testMath = TestMath()
    val s = SimpleShape(math = testMath, transform = Matrix4x4.Translation(5, 0, 0))

    s.intersect(r)

    assert(testMath.savedRay.origin ==~ Point(-5, 0, -5))
    assert(testMath.savedRay.direction ==~ Vector(0, 0, 1))
  }

  test("Computing the normal on a translated shape") {
    val testMath = TestMath()
    val s = SimpleShape(math = testMath, transform = Matrix4x4.Translation(0, 1, 0))

    val point = Point(0, 1.70711, -0.70711)
    //    val n = s.normalAt(point, null)
    val n = Intersection(Double.NaN, s, s.transform :: Nil).myNormalAt(s, point)

    assert(n ==~ Vector(0, 0.70711, -0.70711))
  }

  test("Computing the normal on a transformed shape") {
    val s = SimpleShape(math = TestMath(), transform = Matrix4x4.Scaling(1, 0.5, 1) * Matrix4x4.RotationZ(Pi / 5))

    val point = Point(0, Sqrt2Div2, -Sqrt2Div2)
    //    val n = s.normalAt(point, null)
    val n = Intersection(Double.NaN, s, s.transform :: Nil).myNormalAt(s, point)

    assert(n ==~ Vector(0, 0.97014, -0.24254))
  }
}
