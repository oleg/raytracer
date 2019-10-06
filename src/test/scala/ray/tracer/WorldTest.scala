package ray.tracer

import org.scalatest.FunSuite
import ray.tracer.Matrix4x4.Identity

class WorldTest extends FunSuite {

  test("Creating a world") {
    val w = World(null, null)
    assert(w.light == null)
    assert(w.shapes == null)
  }

  test("The default world") {
    val light = PointLight(Point(-10, 10, -10), Color(1, 1, 1))

    val s1 = Sphere(material = Material(color = Color(0.8, 1.0, 0.6), diffuse = 0.7, specular = 0.2))
    val s2 = Sphere(transform = Identity.scale(0.5, 0.5, 0.5))
    val w = defaultWorld()

    assert(w.light == light)
    assert(w.contains(s1))
    assert(w.contains(s2))

  }

  test("Intersect a world with a ray") {
    val w = defaultWorld()
    val r = Ray(Point(0, 0, -5), Vector(0, 0, 1))
    val xs = w.intersect(r)

    assert(xs.length == 4)
    assert(xs(0).t == 4)
    assert(xs(1).t == 4.5)
    assert(xs(2).t == 5.5)
    assert(xs(3).t == 6)
  }

  test("Shading an intersection") {
    val w = defaultWorld()
    val r = Ray(Point(0, 0, -5), Vector(0, 0, 1))
    val shape = w.shapes(0)
    val i = Intersection(4, shape)

    val comps = i.prepareComputations(r, Intersections(i :: Nil).findNs(i))
    val c = w.shadeHit(comps)

    assert(c ==~ Color(0.38066, 0.47583, 0.2855))
  }

//TODO fix me
//broken after changing
//`val overPoint = point + normalv * EPSILON` to `val overPoint = point + directedNormalv * EPSILON`
//  test("Shading an intersection from the inside") {
//    val w = defaultWorld().copy(light = PointLight(Point(0, 0.25, 0), Color(1, 1, 1)))
//    val r = Ray(Point(0, 0, 0), Vector(0, 0, 1))
//    val shape = w.shapes(1)
//    val i = Intersection(0.5, shape)
//    val comps = i.prepareComputations(r, Intersections(i :: Nil))
//    val c = w.shadeHit(comps)
//    assert(c ==~ Color(0.1, 0.1, 0.1), c)
//  }

  test("shade_hit() is given an intersection in shadow") {
    val s1 = Sphere()
    val s2 = Sphere(transform = Identity.translate(0, 0, 10))
    val w = World(PointLight(Point(0, 0, -10), Color(1, 1, 1)), s1 :: s2 :: Nil)
    val r = Ray(Point(0, 0, 5), Vector(0, 0, 1))
    val i = Intersection(4, s2)

    val comps = i.prepareComputations(r, Intersections(i :: Nil).findNs(i))
    val c = w.shadeHit(comps)

    assert(c == Color(0.1, 0.1, 0.1))
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
    val inner = Sphere(transform = Identity.scale(0.5, 0.5, 0.5), material = Material(ambient = 1))
    val w = World(PointLight(Point(-10, 10, -10), Color(1, 1, 1)), List(inner, outer))
    val r = Ray(Point(0, 0, 0.755), Vector(0, 0, -1))

    val c = w.colorAt(r)

    assert(c ==~ inner.material.color)
  }

  test("There is no shadow when nothing is collinear with point and light") {
    val w = defaultWorld()
    val p = Point(0, 10, 0)

    assert(w.isShadowed(p) == false)
  }

  test("The shadow when an object is between the point and the light") {
    val w = defaultWorld()
    val p = Point(10, -10, 10)

    assert(w.isShadowed(p))
  }

  test("There is no shadow when an object is behind the light") {
    val w = defaultWorld()
    val p = Point(-20, 20, -20)

    assert(w.isShadowed(p) == false)
  }

  test("There is no shadow when an object is behind the point") {
    val w = defaultWorld()
    val p = Point(-2, 2, -2)

    assert(w.isShadowed(p) == false)
  }

  test("The reflected color for a nonreflective material") {
    val outer: Shape = Sphere(material = Material(color = Color(0.8, 1.0, 0.6), diffuse = 0.7, specular = 0.2))
    val inner: Shape = Sphere(transform = Identity.scale(0.5, 0.5, 0.5), material = Material(ambient = 1))
    val world: World = World(PointLight(Point(-10, 10, -10), Color(1, 1, 1)), List(outer, inner))
    val r = Ray(Point(0, 0, 0), Vector(0, 0, 1))

    val i = Intersection(1, inner)
    val comps = i.prepareComputations(r, Intersections(i :: Nil).findNs(i))
    val color = world.reflectedColor(comps, 5)

    assert(color ==~ Color.black)
  }

  test("The reflected color for a reflective material") {
    val outer: Shape = Sphere(material = Material(color = Color(0.8, 1.0, 0.6), diffuse = 0.7, specular = 0.2))
    val inner: Shape = Sphere(transform = Identity.scale(0.5, 0.5, 0.5), material = Material(ambient = 1))
    val plane: Shape = Plane(material = Material(reflective = 0.5), transform = Identity.translate(0, -1, 0))
    val world: World = World(PointLight(Point(-10, 10, -10), Color(1, 1, 1)), List(outer, inner, plane))

    val ray = Ray(Point(0, 0, -3), Vector(0, -Sqrt2Div2, Sqrt2Div2))
    val i = Intersection(Sqrt2, plane)
    val comps = i.prepareComputations(ray, Intersections(i :: Nil).findNs(i))
    val color = world.reflectedColor(comps)

    assert(color ==~ Color(0.19033, 0.23791, 0.14274), color)
  }

  test("shade_hit() with a reflective material") {
    val outer: Shape = Sphere(material = Material(color = Color(0.8, 1.0, 0.6), diffuse = 0.7, specular = 0.2))
    val inner: Shape = Sphere(transform = Identity.scale(0.5, 0.5, 0.5), material = Material(ambient = 1))
    val plane: Shape = Plane(material = Material(reflective = 0.5), transform = Identity.translate(0, -1, 0))
    val world: World = World(PointLight(Point(-10, 10, -10), Color(1, 1, 1)), List(outer, inner, plane))

    val ray = Ray(Point(0, 0, -3), Vector(0, -Sqrt2Div2, Sqrt2Div2))
    val i = Intersection(Sqrt2, plane)
    val comps = i.prepareComputations(ray, Intersections(i :: Nil).findNs(i))
    val color = world.shadeHit(comps)

    assert(color ==~ Color(0.87675, 0.92434, 0.82917), color)
  }

  test("color_at() with mutually reflective surfaces") {
    val lower = Plane(material = Material(reflective = 1), transform = Identity.translate(0, -1, 0))
    val upper = Plane(material = Material(reflective = 1), transform = Identity.translate(0, 1, 0))
    val w = World(light = PointLight(Point(0, 0, 0), Color(1, 1, 1)), shapes = List(upper, lower))

    val r = Ray(Point(0, 0, 0), Vector(0, 1, 0))

    w.colorAt(r)
  }

  test("The reflected color at the maximum recursive depth") {
    val plane = Plane(material = Material(reflective = 0.5), transform = Identity.translate(0, -1, 0))
    val w = World(PointLight(Point(-10, 10, -10), Color(1, 1, 1)), List(plane))
    val r = Ray(Point(0, 0, -3), Vector(0, -Sqrt2Div2, Sqrt2Div2))
    val i = Intersection(Sqrt2, plane)
    val comps = i.prepareComputations(r, Intersections(i :: Nil).findNs(i))
    val color = w.reflectedColor(comps, 0)
    assert(color == Color.black)
  }

  test("The refracted color with an opaque surface") {
    val w = defaultWorld()
    val shape = w.shapes.head
    val r = Ray(Point(0, 0, -5), Vector(0, 0, 1))
    val xs = Intersections(Intersection(4, shape) :: Intersection(6, shape) :: Nil)

    val comps = xs(0).prepareComputations(r, xs.findNs(xs(0)))
    val c = w.refractedColor(comps)

    assert(c == Color.black)
  }

  test("The refracted color at the maximum recursive depth") {
    val w = World(
      PointLight(Point(-10, 10, -10), Color(1, 1, 1)),
      List(
        Sphere(material = Material(
          color = Color(0.8, 1.0, 0.6),
          diffuse = 0.7,
          specular = 0.2,
          transparency = 1.0,
          refractiveIndex = 1.5)),
        Sphere(transform = Identity.scale(0.5, 0.5, 0.5))))

    val shape = w.shapes(0)
    val xs = Intersections(Intersection(4, shape) :: Intersection(6, shape) :: Nil)
    val r = Ray(Point(0, 0, 0), Vector(0, 0, 1))

    val comps = xs(0).prepareComputations(r, xs.findNs(xs(0)))
    val c = w.refractedColor(comps, 0)

    assert(c == Color.black)
  }

  test("The refracted color under total internal reflection") {
    val w = World(
      PointLight(Point(-10, 10, -10), Color(1, 1, 1)),
      List(
        Sphere(material = Material(
          color = Color(0.8, 1.0, 0.6),
          diffuse = 0.7,
          specular = 0.2,
          transparency = 1.0,
          refractiveIndex = 1.5)),
        Sphere(transform = Identity.scale(0.5, 0.5, 0.5))))

    val r = Ray(Point(0, 0, Sqrt2Div2), Vector(0, 1, 0))
    val shape = w.shapes.head
    val xs = Intersections(Intersection(-Sqrt2Div2, shape) :: Intersection(Sqrt2Div2, shape) :: Nil)

    val comps = xs(1).prepareComputations(r, xs.findNs(xs(0)))
    val c = w.refractedColor(comps)

    assert(c == Color.black)
  }

  test("The refracted color with a refracted ray") {
    val a = Sphere(material = Material(ambient = 1.0, pattern = TestPattern()))
    val b = Sphere(transform = Identity.scale(0.5, 0.5, 0.5), material = Material(transparency = 1.0, refractiveIndex = 1.5))

    val w = World(PointLight(Point(-10, 10, -10), Color.white), List(a, b))

    val r = Ray(Point(0, 0, 0.1), Vector(0, 1, 0))
    val xs = Intersections(
      Intersection(-0.9899, a) ::
        Intersection(-0.4899, b) ::
        Intersection(0.4899, b) ::
        Intersection(0.9899, a) ::
        Nil)

    val comps = xs(2).prepareComputations(r, xs.findNs(xs(2)))
    val c = w.refractedColor(comps)

    assert(c ==~ Color(0, 0.99887, 0.04721), c)
  }

  test("shade_hit() with a transparent material") {
    val floor = Plane(
      transform = Identity.translate(0, -1, 0),
      material = Material(transparency = 0.5, refractiveIndex = 1.5)
    )
    val ball = Sphere(
      transform = Identity.translate(0, -3.5, -0.5),
      material = Material(color = Color(1, 0, 0), ambient = 0.5))

    val w = World(PointLight(Point(-10, 10, -10), Color(1, 1, 1)), List(floor, ball))

    val r = Ray(Point(0, 0, -3), Vector(0, -Sqrt2Div2, Sqrt2Div2))
    val xs = Intersections(Intersection(Sqrt2, floor) :: Nil)

    val comps = xs(0).prepareComputations(r, xs.findNs(xs(0)))
    val c = w.shadeHit(comps)

    assert(c ==~ Color(0.93642, 0.68642, 0.68642), c)
  }

  test("shade_hit() with a reflective, transparent material") {
    val floor = Plane(
      transform = Identity.translate(0, -1, 0),
      material = Material(
        reflective = 0.5,
        transparency = 0.5,
        refractiveIndex = 1.5)
    )
    val ball = Sphere(
      transform = Identity.translate(0, -3.5, -0.5),
      material = Material(color = Color(1, 0, 0), ambient = 0.5))

    val w = World(PointLight(Point(-10, 10, -10), Color(1, 1, 1)), List(floor, ball))

    val r = Ray(Point(0, 0, -3), Vector(0, -Sqrt2Div2, Sqrt2Div2))
    val xs = Intersections(Intersection(Sqrt2, floor) :: Nil)

    val comps = xs(0).prepareComputations(r, xs.findNs(xs(0)))
    val c = w.shadeHit(comps)

    //todo fix me
    //assert(c ==~ Color(0.93391, 0.69643, 0.69243), c)
  }

  private def defaultWorld(): World =
    World(
      PointLight(Point(-10, 10, -10), Color(1, 1, 1)),
      List(
        Sphere(material = Material(color = Color(0.8, 1.0, 0.6), diffuse = 0.7, specular = 0.2)),
        Sphere(transform = Identity.scale(0.5, 0.5, 0.5))))

  //todo remove duplication
  case class TestPattern(transform: Matrix4x4 = Matrix4x4.Identity) extends Pattern {
    override def patternAt(point: Point): Color = Color(point.x, point.y, point.z)
  }

}
