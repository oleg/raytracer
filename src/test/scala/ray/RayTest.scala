package ray

import org.scalatest.FunSuite

class RayTest extends FunSuite {

  test("Creating and querying a ray") {
    val origin = Point(1, 2, 3)
    val direction = Vector(4, 5, 6)

    val ray = Ray(origin, direction)

    assert(ray.origin == origin)
    assert(ray.direction == direction)
  }

  test("Computing a point from a distance") {
    val ray = Ray(Point(2, 3, 4), Vector(1, 0, 0))

    assert(ray.position(0) == Point(2, 3, 4))
    assert(ray.position(1) == Point(3, 3, 4))
    assert(ray.position(-1) == Point(1, 3, 4))
    assert(ray.position(2.5) == Point(4.5, 3, 4))
  }

  test("Translating a ray") {
    val r1 = Ray(Point(1, 2, 3), Vector(0, 1, 0))

    val r2: Ray = r1.transform(Matrix4x4.Translation(3, 4, 5))

    assert(r2.origin == Point(4, 6, 8))
    assert(r2.direction == Vector(0, 1, 0))
  }

  test("Scaling a ray") {
    val r1 = Ray(Point(1, 2, 3), Vector(0, 1, 0))

    val r2: Ray = r1.transform(Matrix4x4.Scaling(2, 3, 4))

    assert(r2.origin == Point(2, 6, 12))
    assert(r2.direction == Vector(0, 3, 0))
  }

}

