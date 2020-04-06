package ray.tracer

import org.scalatest.funsuite.AnyFunSuite
import ray.tracer.shape.ShapeFactory._
import ray.tracer.shape.{Csg, Operation}

class CsgTest extends AnyFunSuite {

  test("CSG is created with an operation and two shapes") {
    val s1 = Sphere()
    val s2 = Cube()

    val c = Csg(Operation.union, s1, s2)

    assert(c.operation == Operation.union)
    assert(c.left == s1)
    assert(c.right == s2)
  }

  test("Evaluating the rule for a CSG union") {
    //intersectionAllowed(op, lhit, inl, inr) == result

    assert(Operation.intersectionAllowed(Operation.union, true, true, true) == false)
    assert(Operation.intersectionAllowed(Operation.union, true, true, false) == true)
    assert(Operation.intersectionAllowed(Operation.union, true, false, true) == false)
    assert(Operation.intersectionAllowed(Operation.union, true, false, false) == true)
    assert(Operation.intersectionAllowed(Operation.union, false, true, true) == false)
    assert(Operation.intersectionAllowed(Operation.union, false, true, false) == false)
    assert(Operation.intersectionAllowed(Operation.union, false, false, true) == true)
    assert(Operation.intersectionAllowed(Operation.union, false, false, false) == true)
  }

  test("Evaluating the rule for a CSG intersection") {
    //intersectionAllowed(op, lhit, inl, inr) == result

    assert(Operation.intersectionAllowed(Operation.intersection, true, true, true) == true)
    assert(Operation.intersectionAllowed(Operation.intersection, true, true, false) == false)
    assert(Operation.intersectionAllowed(Operation.intersection, true, false, true) == true)
    assert(Operation.intersectionAllowed(Operation.intersection, true, false, false) == false)
    assert(Operation.intersectionAllowed(Operation.intersection, false, true, true) == true)
    assert(Operation.intersectionAllowed(Operation.intersection, false, true, false) == true)
    assert(Operation.intersectionAllowed(Operation.intersection, false, false, true) == false)
    assert(Operation.intersectionAllowed(Operation.intersection, false, false, false) == false)
  }

  test("Evaluating the rule for a CSG difference") {
    //intersectionAllowed(op, lhit, inl, inr) == result

    assert(Operation.intersectionAllowed(Operation.difference, true, true, true) == false)
    assert(Operation.intersectionAllowed(Operation.difference, true, true, false) == true)
    assert(Operation.intersectionAllowed(Operation.difference, true, false, true) == false)
    assert(Operation.intersectionAllowed(Operation.difference, true, false, false) == true)
    assert(Operation.intersectionAllowed(Operation.difference, false, true, true) == true)
    assert(Operation.intersectionAllowed(Operation.difference, false, true, false) == true)
    assert(Operation.intersectionAllowed(Operation.difference, false, false, true) == false)
    assert(Operation.intersectionAllowed(Operation.difference, false, false, false) == false)
  }

  test("Filtering a list of intersections for union") {
    val s1 = Sphere()
    val s2 = Cube()
    val c = Csg(Operation.union, s1, s2)
    val xs = Intersections(Intersection(1, s1) :: Intersection(2, s2) :: Intersection(3, s1) :: Intersection(4, s2) :: Nil)

    val result = c.filterIntersections(xs)
    assert(result.length == 2)
    assert(result(0) == xs(0))
    assert(result(1) == xs(3))

  }

  test("Filtering a list of intersections for intersection") {
    val s1 = Sphere()
    val s2 = Cube()
    val c = Csg(Operation.intersection, s1, s2)
    val xs = Intersections(Intersection(1, s1) :: Intersection(2, s2) :: Intersection(3, s1) :: Intersection(4, s2) :: Nil)

    val result = c.filterIntersections(xs)
    assert(result.length == 2)
    assert(result(0) == xs(1))
    assert(result(1) == xs(2))

  }

  test("Filtering a list of intersections for difference") {
    val s1 = Sphere()
    val s2 = Cube()
    val c = Csg(Operation.difference, s1, s2)
    val xs = Intersections(Intersection(1, s1) :: Intersection(2, s2) :: Intersection(3, s1) :: Intersection(4, s2) :: Nil)

    val result = c.filterIntersections(xs)
    assert(result.length == 2)
    assert(result(0) == xs(0))
    assert(result(1) == xs(1))
  }

  test("A ray misses a CSG object") {
    val c = Csg(Operation.union, Sphere(), Cube())
    val r = Ray(Point(0, 2, -5), Vector(0, 0, 1))

    val xs = c.localIntersect(r)

    assert(xs.isEmpty)
  }

  test("A ray hits a CSG object") {
    val s1 = Sphere()
    val s2 = Sphere(transform = Matrix4x4.Translation(0, 0, 0.5))
    val c = Csg(Operation.union, s1, s2)
    val r = Ray(Point(0, 0, -5), Vector(0, 0, 1))

    val xs = c.localIntersect(r)

    assert(xs.length == 2)
    assert(xs(0).t == 4)
    assert(xs(0).obj == s1)
    assert(xs(1).t == 6.5)
    assert(xs(1).obj == s2)
  }

}
