package ray.tracer

import org.scalatest.FunSuite

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

}
