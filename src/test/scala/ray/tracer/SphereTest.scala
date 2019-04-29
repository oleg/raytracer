package ray.tracer

import org.scalatest.FunSuite

import scala.math.Pi

class SphereTest extends FunSuite {

  test("A ray intersects a sphere at two points") {
    val ray = Ray(Point(0, 0, -5), Vector(0, 0, 1))
    val sphere = Sphere()

    val data = sphere.intersect(ray)
    assert(data.length == 2)
    assert(data(0).t == 4.0)
    assert(data(1).t == 6.0)
  }

  test("A ray intersects a sphere at a tangent") {
    val ray = Ray(Point(0, 1, -5), Vector(0, 0, 1))
    val sphere = Sphere()

    val data = sphere.intersect(ray)
    assert(data.length == 2)
    assert(data(0).t == 5.0)
    assert(data(1).t == 5.0)
  }

  test("A ray misses a sphere") {
    val ray = Ray(Point(0, 2, -5), Vector(0, 0, 1))
    val sphere = Sphere()

    val data = sphere.intersect(ray)
    assert(data.length == 0)
  }

  test("A ray originates inside a sphere") {
    val ray = Ray(Point(0, 0, 0), Vector(0, 0, 1))
    val sphere = Sphere()

    val data = sphere.intersect(ray)
    assert(data.length == 2)
    assert(data(0).t == -1.0)
    assert(data(1).t == 1.0)
  }

  test("A sphere is behind a ray") {
    val ray = Ray(Point(0, 0, 5), Vector(0, 0, 1))
    val sphere = Sphere()

    val data = sphere.intersect(ray)
    assert(data.length == 2)
    assert(data(0).t == -6.0)
    assert(data(1).t == -4.0)
  }

  test("Intersect sets the object on the intersection") {
    val ray = Ray(Point(0, 0, -5), Vector(0, 0, 1))
    val sphere = Sphere()

    val data = sphere.intersect(ray)
    assert(data.length == 2)
    assert(data(0).obj == sphere)
    assert(data(1).obj == sphere)
  }

  test("A sphere's default transformation") {
    val sphere = Sphere()

    assert(sphere.transform == Matrix4x4.Identity)
  }

  test("Changing a sphere's transformation") {
    val t = Matrix4x4.Translation(2, 3, 4)
    val sphere = Sphere(transform = t)

    assert(sphere.transform == t)
  }

  test("Intersecting a scaled sphere with a ray") {
    val r = Ray(Point(0, 0, -5), Vector(0, 0, 1))
    val s = Sphere(transform = Matrix4x4.Scaling(2, 2, 2))

    val xs = s.intersect(r)
    assert(xs.length == 2)
    assert(xs(0).t == 3)
    assert(xs(1).t == 7)
  }

  test("Intersecting a translated sphere with a ray") {
    val r = Ray(Point(0, 0, -5), Vector(0, 0, 1))
    val s = Sphere(transform = Matrix4x4.Translation(5, 0, 0))

    val xs = s.intersect(r)
    assert(xs.length == 0)
  }

  test("The normal on a sphere at a point on the x axis") {
    val s = Sphere()
    val n = s.normalAt(Point(1, 0, 0))
    assert(n == Vector(1, 0, 0))
  }

  test("The normal on a sphere at a point on the y axis") {
    val s = Sphere()
    val n = s.normalAt(Point(0, 1, 0))
    assert(n == Vector(0, 1, 0))
  }

  test("The normal on a sphere at a point on the z axis") {
    val s = Sphere()
    val n = s.normalAt(Point(0, 0, 1))
    assert(n == Vector(0, 0, 1))
  }

  test("The normal on a sphere at a nonaxial point") {
    val s = Sphere()
    val v = math.sqrt(3) / 3

    val n = s.normalAt(Point(v, v, v))

    assert(n == Vector(v, v, v))
  }

  test("The normal is a normalized vector") {
    val s = Sphere()
    val v = math.sqrt(3) / 3

    val n = s.normalAt(Point(v, v, v))

    assert(n == n.normalize)
  }

  test("Computing the normal on a translated sphere") {
    val s = Sphere(transform = Matrix4x4.Translation(0, 1, 0))

    val n = s.normalAt(Point(0, 1.70711, -0.70711))

    assert(n ==~ Vector(0, 0.70711, -0.70711))
  }

  test("Computing the normal on a transformed sphere") {
    val s = Sphere(transform = Matrix4x4.Scaling(1, 0.5, 1) * Matrix4x4.RotationZ(Pi / 5))
    val v = math.sqrt(2) / 2

    val n = s.normalAt(Point(0, v, -v))

    assert(n ==~ Vector(0, 0.97014, -0.24254))
  }

  test("A sphere has a default material") {
    val s = Sphere()

    assert(s.material == Material())
  }

  test("A sphere may be assigned a material") {
    val m = Material(ambient = 1)
    val s = Sphere(material = m)
    assert(s.material == m)
  }

}
