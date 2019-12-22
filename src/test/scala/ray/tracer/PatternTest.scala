package ray.tracer

import org.scalatest.FunSuite
import ray.tracer.Matrix4x4.{Scaling, Translation}
import ray.tracer.Shape.Sphere
class PatternTest extends FunSuite {

  case class TestPattern(transform: Matrix4x4 = Matrix4x4.Identity) extends Pattern {
    override def patternAt(point: Point): Color = Color(point.x, point.y, point.z)
  }

  test("The default pattern transformation") {
    val pattern = TestPattern()
    assert(pattern.transform == Matrix4x4.Identity)
  }

  test("Assigning a transformation") {
    val pattern = TestPattern(transform = Translation(1, 2, 3))
    assert(pattern.transform == Translation(1, 2, 3))
  }

  test("A pattern with an object transformation") {
    val shape = Sphere(transform = Scaling(2, 2, 2))
    val pattern = TestPattern()

    val c = pattern.patternAtShape(shape, Point(2, 3, 4))

    assert(c == Color(1, 1.5, 2))
  }

  test("A pattern with a pattern transformation") {
    val shape = Sphere()
    val pattern = TestPattern(transform = Scaling(2, 2, 2))

    val c = pattern.patternAtShape(shape, Point(2, 3, 4))

    assert(c == Color(1, 1.5, 2))
  }

  test("A pattern with both an object and a pattern transformation") {
    val shape = Sphere(transform = Scaling(2, 2, 2))
    val pattern = TestPattern(transform = Translation(0.5, 1, 1.5))

    val c = pattern.patternAtShape(shape, Point(2.5, 3, 3.5))

    assert(c == Color(0.75, 0.5, 0.25))
  }

  test("Creating a stripe pattern") {
    val pattern = StripePattern(Color.white, Color.black)

    assert(pattern.a == Color.white)
    assert(pattern.b == Color.black)
  }

  test("A stripe pattern is constant in y") {
    val pattern = StripePattern(Color.white, Color.black)

    assert(pattern.patternAt(Point(0, 0, 0)) == Color.white)
    assert(pattern.patternAt(Point(0, 1, 0)) == Color.white)
    assert(pattern.patternAt(Point(0, 2, 0)) == Color.white)
  }

  test("A stripe pattern is constant in z") {
    val pattern = StripePattern(Color.white, Color.black)

    assert(pattern.patternAt(Point(0, 0, 0)) == Color.white)
    assert(pattern.patternAt(Point(0, 0, 1)) == Color.white)
    assert(pattern.patternAt(Point(0, 0, 2)) == Color.white)
  }

  test("A stripe pattern alternates in x") {
    val pattern = StripePattern(Color.white, Color.black)

    assert(pattern.patternAt(Point(0, 0, 0)) == Color.white)
    assert(pattern.patternAt(Point(0.9, 0, 0)) == Color.white)

    assert(pattern.patternAt(Point(1, 0, 0)) == Color.black)
    assert(pattern.patternAt(Point(-0.1, 0, 0)) == Color.black)

    assert(pattern.patternAt(Point(-1, 0, 0)) == Color.black)
    assert(pattern.patternAt(Point(-1.1, 0, 0)) == Color.white)
  }

  test("Stripes with an object transformation") {
    val obj = Sphere(transform = Scaling(2, 2, 2))
    val pattern = StripePattern(Color.white, Color.black)

    val c = pattern.patternAtShape(obj, Point(1.5, 0, 0))

    assert(c == Color.white)
  }

  test("Stripes with a pattern transformation") {
    val obj = Sphere()
    val pattern = StripePattern(Color.white, Color.black, transform = Scaling(2, 2, 2))

    val c = pattern.patternAtShape(obj, Point(1.5, 0, 0))

    assert(c == Color.white)
  }

  test("Stripes with both an object and a pattern transformation") {
    val obj = Sphere(transform = Scaling(2, 2, 2))
    val pattern = StripePattern(Color.white, Color.black, transform = Translation(0.5, 0, 0))

    val c = pattern.patternAtShape(obj, Point(2.5, 0, 0))

    assert(c == Color.white)
  }

  test("A gradient linearly interpolates between colors") {
    val pattern = GradientPattern(Color.white, Color.black)

    assert(pattern.patternAt(Point(0, 0, 0)) == Color.white)
    assert(pattern.patternAt(Point(0.25, 0, 0)) == Color(0.75, 0.75, 0.75))
    assert(pattern.patternAt(Point(0.5, 0, 0)) == Color(0.5, 0.5, 0.5))
    assert(pattern.patternAt(Point(0.75, 0, 0)) == Color(0.25, 0.25, 0.25))
  }

  test("A ring should extend in both x and z") {
    val pattern = RingPattern(Color.white, Color.black)

    assert(pattern.patternAt(Point(0, 0, 0)) == Color.white)
    assert(pattern.patternAt(Point(1, 0, 0)) == Color.black)
    assert(pattern.patternAt(Point(0, 0, 1)) == Color.black)
    assert(pattern.patternAt(Point(0.708, 0, 0.708)) == Color.black)
  }

  test("Checkers should repeat in x") {
    val pattern = CheckersPattern(Color.white, Color.black)

    assert(pattern.patternAt(Point(0, 0, 0)) == Color.white)
    assert(pattern.patternAt(Point(0.99, 0, 0)) == Color.white)
    assert(pattern.patternAt(Point(1.01, 0, 0)) == Color.black)
  }

  test("Checkers should repeat in y") {
    val pattern = CheckersPattern(Color.white, Color.black)

    assert(pattern.patternAt(Point(0, 0, 0)) == Color.white)
    assert(pattern.patternAt(Point(0, 0.99, 0)) == Color.white)
    assert(pattern.patternAt(Point(0, 1.01, 0)) == Color.black)
  }

  test("Checkers should repeat in z") {
    val pattern = CheckersPattern(Color.white, Color.black)

    assert(pattern.patternAt(Point(0, 0, 0)) == Color.white)
    assert(pattern.patternAt(Point(0, 0, 0.99)) == Color.white)
    assert(pattern.patternAt(Point(0, 0, 1.01)) == Color.black)
  }
}
