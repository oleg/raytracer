package ray

import org.scalatest.FunSuite

class WorldTest extends FunSuite {

  test("Creating a world") {
    val w = World(null, null)
    assert(w.light == null)
    assert(w.spheres == null)
  }

  test("The default world") {
    val light = PointLight(Point(-10, 10, -10), Color(1, 1, 1))

    val s1 = Sphere(material = Material(color = Color(0.8, 1.0, 0.6), diffuse = 0.7, specular = 0.2))
    val s2 = Sphere(transform = Matrix4x4.Identity.scale(0.5, 0.5, 0.5))
    val w = defaultWorld()

    assert(w.light == light)
    assert(w.contains(s1))
    assert(w.contains(s2))

  }

  test("Intersect a world with a ray") {
    val w = defaultWorld()
    val r = Ray(Point(0, 0, -5), Vector(0, 0, 1))
    val xs = r.intersect(w)

    assert(xs.length == 4)
    assert(xs(0).t == 4)
    assert(xs(1).t == 4.5)
    assert(xs(2).t == 5.5)
    assert(xs(3).t == 6)
  }

  test("Shading an intersection") {
    val w = defaultWorld()
    val r = Ray(Point(0, 0, -5), Vector(0, 0, 1))
    val shape = w.spheres(0)
    val i = Intersection(4, shape)

    val comps = i.prepareComputations(r)
    val c = w.shadeHit(comps)

    assert(c ==~ Color(0.38066, 0.47583, 0.2855))
  }

  test("Shading an intersection from the inside") {
    val w = defaultWorld().copy(light = PointLight(Point(0, 0.25, 0), Color(1, 1, 1)))
    val r = Ray(Point(0, 0, 0), Vector(0, 0, 1))
    val shape = w.spheres(1)
    val i = Intersection(0.5, shape)

    val comps = i.prepareComputations(r)
    val c = w.shadeHit(comps)
    assert(c ==~ Color(0.90498, 0.90498, 0.90498))
  }

  test("The color when a ray misses") {
    val w = defaultWorld()
    val r = Ray(Point(0, 0, -5), Vector(0, 1, 0))

    val c = w.colorAt(r)

    assert(c ==~ Color(0, 0, 0))
  }

  test("The color when a ray hits") {
    val w = defaultWorld()
    val r = Ray(Point(0, 0, -5), Vector(0, 0, 1))

    val c = w.colorAt(r)

    assert(c ==~ Color(0.38066, 0.47583, 0.2855))
  }

  test("The color with an intersection behind the ray") {
    val outer = Sphere(material = Material(color = Color(0.8, 1.0, 0.6), diffuse = 0.7, specular = 0.2, ambient = 1))
    val inner = Sphere(transform = Matrix4x4.Identity.scale(0.5, 0.5, 0.5), material = Material(ambient = 1))
    val w = World(PointLight(Point(-10, 10, -10), Color(1, 1, 1)), List(inner, outer))
    val r = Ray(Point(0, 0, 0.755), Vector(0, 0, -1))

    val c = w.colorAt(r)

    assert(c ==~ inner.material.color)
  }

  private def defaultWorld(): World = World(
    PointLight(Point(-10, 10, -10), Color(1, 1, 1)),
    List(
      Sphere(material = Material(color = Color(0.8, 1.0, 0.6), diffuse = 0.7, specular = 0.2)),
      Sphere(transform = Matrix4x4.Identity.scale(0.5, 0.5, 0.5))))

}
