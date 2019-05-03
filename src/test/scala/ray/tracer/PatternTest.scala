package ray.tracer

import org.scalatest.FunSuite
import ray.tracer.Matrix4x4.{Scaling, Translation}

class PatternTest extends FunSuite {

  test("Creating a stripe pattern") {
    val pattern = StripePattern(Color.white, Color.black)

    assert(pattern.a == Color.white)
    assert(pattern.b == Color.black)
  }

  test("A stripe pattern is constant in y") {
    val pattern = StripePattern(Color.white, Color.black)

    assert(pattern.stripeAt(Point(0, 0, 0)) == Color.white)
    assert(pattern.stripeAt(Point(0, 1, 0)) == Color.white)
    assert(pattern.stripeAt(Point(0, 2, 0)) == Color.white)
  }

  test("A stripe pattern is constant in z") {
    val pattern = StripePattern(Color.white, Color.black)

    assert(pattern.stripeAt(Point(0, 0, 0)) == Color.white)
    assert(pattern.stripeAt(Point(0, 0, 1)) == Color.white)
    assert(pattern.stripeAt(Point(0, 0, 2)) == Color.white)
  }

  test("A stripe pattern alternates in x") {
    val pattern = StripePattern(Color.white, Color.black)

    assert(pattern.stripeAt(Point(0, 0, 0)) == Color.white)
    assert(pattern.stripeAt(Point(0.9, 0, 0)) == Color.white)

    assert(pattern.stripeAt(Point(1, 0, 0)) == Color.black)
    assert(pattern.stripeAt(Point(-0.1, 0, 0)) == Color.black)

    assert(pattern.stripeAt(Point(-1, 0, 0)) == Color.black)
    assert(pattern.stripeAt(Point(-1.1, 0, 0)) == Color.white)
  }

  test("Stripes with an object transformation") {
    val obj = Sphere(transform = Scaling(2, 2, 2))
    val pattern = StripePattern(Color.white, Color.black)

    val c = pattern.stripeAtObject(obj, Point(1.5, 0, 0))

    assert(c == Color.white)
  }

  test("Stripes with a pattern transformation") {
    val obj = Sphere()
    val pattern = StripePattern(Color.white, Color.black, transform = Scaling(2, 2, 2))

    val c = pattern.stripeAtObject(obj, Point(1.5, 0, 0))

    assert(c == Color.white)
  }

  test("Stripes with both an object and a pattern transformation") {
    val obj = Sphere(transform = Scaling(2, 2, 2))
    val pattern = StripePattern(Color.white, Color.black, transform = Translation(0.5, 0, 0))

    val c = pattern.stripeAtObject(obj, Point(2.5, 0, 0))

    assert(c == Color.white)
  }

}
