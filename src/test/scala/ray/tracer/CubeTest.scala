package ray.tracer

import org.scalatest.FunSuite

class CubeTest extends FunSuite {

  test("A ray intersects a cube") {
    val c = Cube()

    assertIntersection(c.localIntersect(Ray(Point(5, 0.5, 0), Vector(-1, 0, 0))), t1 = 4, t2 = 6)
    assertIntersection(c.localIntersect(Ray(Point(-5, 0.5, 0), Vector(1, 0, 0))), t1 = 4, t2 = 6)
    assertIntersection(c.localIntersect(Ray(Point(0.5, 5, 0), Vector(0, -1, 0))), t1 = 4, t2 = 6)
    assertIntersection(c.localIntersect(Ray(Point(0.5, -5, 0), Vector(0, 1, 0))), t1 = 4, t2 = 6)
    assertIntersection(c.localIntersect(Ray(Point(0.5, 0, 5), Vector(0, 0, -1))), t1 = 4, t2 = 6)
    assertIntersection(c.localIntersect(Ray(Point(0.5, 0, -5), Vector(0, 0, 1))), t1 = 4, t2 = 6)
    assertIntersection(c.localIntersect(Ray(Point(0, 0.5, 0), Vector(0, 0, 1))), t1 = -1, t2 = 1)
  }

  test("A ray misses a cube") {
    val c = Cube()
    assert(c.localIntersect(Ray(Point(-2, 0, 0), Vector(0.2673, 0.5345, 0.8018))).isEmpty)
    assert(c.localIntersect(Ray(Point(0, -2, 0), Vector(0.8018, 0.2673, 0.5345))).isEmpty)
    assert(c.localIntersect(Ray(Point(0, 0, -2), Vector(0.5345, 0.8018, 0.2673))).isEmpty)
    assert(c.localIntersect(Ray(Point(2, 0, 2), Vector(0, 0, -1))).isEmpty)
    assert(c.localIntersect(Ray(Point(0, 2, 2), Vector(0, -1, 0))).isEmpty)
    assert(c.localIntersect(Ray(Point(2, 2, 0), Vector(-1, 0, 0))).isEmpty)
  }

  test("The normal on the surface of a cube") {
    val c = Cube()
    assert(c.localNormalAt(Point(1, 0.5, -0.8)) == Vector(1, 0, 0))
    assert(c.localNormalAt(Point(-1, -0.2, 0.9)) == Vector(-1, 0, 0))
    assert(c.localNormalAt(Point(-0.4, 1, -0.1)) == Vector(0, 1, 0))
    assert(c.localNormalAt(Point(0.3, -1, -0.7)) == Vector(0, -1, 0))
    assert(c.localNormalAt(Point(-0.6, 0.3, 1)) == Vector(0, 0, 1))
    assert(c.localNormalAt(Point(0.4, 0.4, -1)) == Vector(0, 0, -1))
    assert(c.localNormalAt(Point(1, 1, 1)) == Vector(1, 0, 0))
    assert(c.localNormalAt(Point(-1, -1, -1)) == Vector(-1, 0, 0))
  }

  private def assertIntersection(intersections: Intersections, t1: Int, t2: Int): Unit = {
    assert(intersections(0).t == t1)
    assert(intersections(1).t == t2)
  }

}
