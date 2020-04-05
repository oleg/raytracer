package ray.tracer

import org.scalatest.funsuite.AnyFunSuite
import ray.tracer.shape.ShapeFactory._

import scala.math.Pi

class CameraTest extends AnyFunSuite {
  val p = implicitly[Precision[Double]]

  test("Constructing a camera") {
    val c = Camera(160, 120, Pi / 2)

    assert(c.hszie == 160)
    assert(c.vsize == 120)
    assert(c.fieldOfView == Pi / 2)
    assert(c.transform == Matrix4x4.Identity)
  }

  test("The pixel size for a horizontal canvas") {
    val c = Camera(200, 125, Pi / 2)
    assert(p.approximatelyEqual(c.pixelSize, 0.01), c.pixelSize)

  }

  test("The pixel size for a vertical canvas") {
    val c = Camera(125, 200, Pi / 2)

    assert(p.approximatelyEqual(c.pixelSize, 0.01), c.pixelSize)
  }

  test("Constructing a ray through the center of the canvas") {
    val c = Camera(201, 101, Pi / 2)
    val r = c.rayForPixel(100, 50)

    assert(r.origin ==~ Point(0, 0, 0))
    assert(r.direction ==~ Vector(0, 0, -1))
  }

  test("Constructing a ray through a corner of the canvas") {
    val c = Camera(201, 101, Pi / 2)
    val r = c.rayForPixel(0, 0)

    assert(r.origin ==~ Point(0, 0, 0))
    assert(r.direction ==~ Vector(0.66519, 0.33259, -0.66851))
  }

  test("Constructing a ray when the camera is transformed") {
    val c = Camera(201, 101, Pi / 2, Matrix4x4.RotationY(Pi / 4) * Matrix4x4.Translation(0, -2, 5))
    val r = c.rayForPixel(100, 50)

    assert(r.origin ==~ Point(0, 2, -5))
    assert(r.direction ==~ Vector(Sqrt2Div2, 0, -Sqrt2Div2))
  }

  test("Rendering a world with a camera") {
    val w = defaultWorld()
    val from = Point(0, 0, -5)
    val to = Point(0, 0, 0)
    val up = Vector(0, 1, 0)
    val c = Camera(11, 11, Pi / 2, Matrix4x4.viewTransform(from, to, up))

    val image = c.render(w)
    assert(image(5, 5) ==~ Color(0.38066, 0.47583, 0.2855))
  }


  private def defaultWorld(): World = World(
    PointLight(Point(-10, 10, -10), Color(1, 1, 1)),
    List(
      Sphere(material = Material(color = Color(0.8, 1.0, 0.6), diffuse = 0.7, specular = 0.2)),
      Sphere(transform = Matrix4x4.Identity.scale(0.5, 0.5, 0.5))))

}
