package ray.tracer

import org.scalatest.FunSuite
import ray.tracer.Matrix4x4.Scaling
import ray.tracer.ShapeFactory._

class MaterialTest extends FunSuite {

  test("The default material") {
    val m = Material()

    assert(m.color == Color(1, 1, 1))
    assert(m.ambient == 0.1)
    assert(m.diffuse == 0.9)
    assert(m.specular == 0.9)
    assert(m.shininess == 200.0)
  }

  test("Lighting with the eye between the light and the surface") {
    val sphere = Sphere()
    val material = Material()
    val position = Point(0, 0, 0)
    val eyev = Vector(0, 0, -1)
    val normalv = Vector(0, 0, -1)
    val light = PointLight(Point(0, 0, -10), Color(1, 1, 1))
    val inShadow = false

    val result = material.lighting(light, sphere, position, eyev, normalv, inShadow)

    assert(result ==~ Color(1.9, 1.9, 1.9), result)
  }

  test("Lighting with the eye between light and surface, eye offset 45 grad") {
    val sphere = Sphere()
    val material = Material()
    val position = Point(0, 0, 0)
    val eyev = Vector(0, Sqrt2Div2, -Sqrt2Div2)
    val normalv = Vector(0, 0, -1)
    val light = PointLight(Point(0, 0, -10), Color(1, 1, 1))
    val inShadow = false

    val result = material.lighting(light, sphere, position, eyev, normalv, inShadow)

    assert(result ==~ Color(1.0, 1.0, 1.0), result)
  }

  test("Lighting with eye opposite surface, light offset 45 grad") {
    val sphere = Sphere()
    val material = Material()
    val position = Point(0, 0, 0)
    val eyev = Vector(0, 0, -1)
    val normalv = Vector(0, 0, -1)
    val light = PointLight(Point(0, 10, -10), Color(1, 1, 1))
    val inShadow = false

    val result = material.lighting(light, sphere, position, eyev, normalv, inShadow)

    assert(result ==~ Color(0.7364, 0.7364, 0.7364), result)
  }

  test("Lighting with eye in the path of the reflection vector") {
    val sphere = Sphere()
    val material = Material()
    val position = Point(0, 0, 0)
    val eyev = Vector(0, -Sqrt2Div2, -Sqrt2Div2)
    val normalv = Vector(0, 0, -1)
    val light = PointLight(Point(0, 10, -10), Color(1, 1, 1))
    val inShadow = false

    val result = material.lighting(light, sphere, position, eyev, normalv, inShadow)

    assert(result ==~ Color(1.6364, 1.6364, 1.6364), result)
  }

  test("Lighting with the light behind the surface") {
    val sphere = Sphere()
    val material = Material()
    val position = Point(0, 0, 0)
    val eyev = Vector(0, 0, -1)
    val normalv = Vector(0, 0, -1)
    val light = PointLight(Point(0, 0, 10), Color(1, 1, 1))
    val inShadow = false

    val result = material.lighting(light, sphere, position, eyev, normalv, inShadow)

    assert(result ==~ Color(0.1, 0.1, 0.1), result)
  }

  test("Lighting with the surface in shadow") {
    val sphere = Sphere()
    val material = Material()
    val position = Point(0, 0, 0)
    val eyev = Vector(0, 0, -1)
    val normalv = Vector(0, 0, -1)
    val light = PointLight(Point(0, 0, -10), Color(1, 1, 1))
    val inShadow = true

    val result = material.lighting(light, sphere, position, eyev, normalv, inShadow)

    assert(result ==~ Color(0.1, 0.1, 0.1), result)
  }

  test("Lighting with a pattern applied") {
    val sphere = Sphere()
    val material = Material(ambient = 1, diffuse = 0, specular = 0, pattern = StripePattern(Color.white, Color.black))
    val position = Point(0, 0, 0)
    val eyev = Vector(0, 0, -1)
    val normalv = Vector(0, 0, -1)
    val light = PointLight(Point(0, 0, -10), Color(1, 1, 1))

    val c1 = material.lighting(light, sphere, Point(0.9, 0, 0), eyev, normalv, false)
    val c2 = material.lighting(light, sphere, Point(1.1, 0, 0), eyev, normalv, false)

    assert(c1 == Color.white)
    assert(c2 == Color.black)
  }

  test("Reflectivity for the default material") {
    val m = Material()

    assert(m.reflective == 0.0)
  }

  test("Transparency for the default material") {
    val m = Material()

    assert(m.transparency == 0.0)
  }

  test("Refractive Index for the default material") {
    val m = Material()

    assert(m.refractiveIndex == 1.0)
  }

  test("A helper for producing a sphere with a glassy material") {
    val s = glassSphere()
    assert(s.transform == Matrix4x4.Identity)
    assert(s.material.transparency == 1.0)
    assert(s.material.refractiveIndex == 1.5)
  }

  test("Scenario Outline: Finding n1 and n2 at various intersections") {
    val a = glassSphere(transform = Scaling(2, 2, 2), material = Material(refractiveIndex = 1.5))
    val b = glassSphere(transform = Scaling(0, 0, -0.25), material = Material(refractiveIndex = 2.0))
    val c = glassSphere(transform = Scaling(0, 0, 0.25), material = Material(refractiveIndex = 2.5))

    val r = Ray(Point(0, 0, -4), Vector(0, 0, 1))

    val xs = Intersections(Intersection(2, a) :: Intersection(2.75, b) :: Intersection(3.25, c) ::
      Intersection(4.75, b) :: Intersection(5.25, c) :: Intersection(6, a) :: Nil)
    //todo fix this test
    val assertNs = (cmp: Computation, n1: Double, n2: Double) => assert((cmp.n1, cmp.n2) == (n1, n2))

    assertNs(xs(0).prepareComputations(r, xs.findNs(xs(0))), 1.0, 1.5)
    assertNs(xs(1).prepareComputations(r,xs.findNs(xs(1))), 1.5, 2.0)
    assertNs(xs(2).prepareComputations(r,xs.findNs(xs(2))), 2.0, 2.5)
    assertNs(xs(3).prepareComputations(r,xs.findNs(xs(3))), 2.5, 2.5)
    assertNs(xs(4).prepareComputations(r,xs.findNs(xs(4))), 2.5, 1.5)
    assertNs(xs(5)prepareComputations(r, xs.findNs(xs(5))), 1.5, 1.0)
  }

}
