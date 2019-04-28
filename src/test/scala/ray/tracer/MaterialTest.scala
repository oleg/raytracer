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
    val material = Material()
    val position = Point(0, 0, 0)
    val eyev = Vector(0, 0, -1)
    val normalv = Vector(0, 0, -1)
    val light = PointLight(Point(0, 0, -10), Color(1, 1, 1))

    val result = material.lighting(light, position, eyev, normalv)

    assert(result ==~ Color(1.9, 1.9, 1.9), result)
  }

  test("Lighting with the eye between light and surface, eye offset 45 grad") {
    val material = Material()
    val position = Point(0, 0, 0)
    val v = math.sqrt(2) / 2
    val eyev = Vector(0, v, -v)
    val normalv = Vector(0, 0, -1)
    val light = PointLight(Point(0, 0, -10), Color(1, 1, 1))

    val result = material.lighting(light, position, eyev, normalv)

    assert(result ==~ Color(1.0, 1.0, 1.0), result)
  }

  test("Lighting with eye opposite surface, light offset 45 grad") {
    val material = Material()
    val position = Point(0, 0, 0)
    val eyev = Vector(0, 0, -1)
    val normalv = Vector(0, 0, -1)
    val light = PointLight(Point(0, 10, -10), Color(1, 1, 1))

    val result = material.lighting(light, position, eyev, normalv)

    assert(result ==~ Color(0.7364, 0.7364, 0.7364), result)
  }

  test("Lighting with eye in the path of the reflection vector") {
    val material = Material()
    val position = Point(0, 0, 0)
    val v = math.sqrt(2) / 2
    val eyev = Vector(0, -v, -v)
    val normalv = Vector(0, 0, -1)
    val light = PointLight(Point(0, 10, -10), Color(1, 1, 1))

    val result = material.lighting(light, position, eyev, normalv)

    assert(result ==~ Color(1.6364, 1.6364, 1.6364), result)
  }

  test("Lighting with the light behind the surface") {
    val material = Material()
    val position = Point(0, 0, 0)
    val eyev = Vector(0, 0, -1)
    val normalv = Vector(0, 0, -1)
    val light = PointLight(Point(0, 0, 10), Color(1, 1, 1))

    val result = material.lighting(light, position, eyev, normalv)

    assert(result ==~ Color(0.1, 0.1, 0.1), result)
  }

}
