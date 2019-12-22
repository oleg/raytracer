package it.ray.tracer

import org.scalatest.FunSuite
import ray.tracer.Matrix4x4.Identity
import ray.tracer._
import testutil.Sources

import scala.math.Pi
import ray.tracer.ShapeFactory._
class CylinderSceneIntegrationTest extends FunSuite {

  test("generate cylinders") {

    val floor = Plane(
      material = Material(
        reflective = 0.7,
        transparency = 0.2,
        refractiveIndex = 1.3),
      transform = Identity.translate(0, -20, 0)
    )

    val c1 = Cylinder(
      transform = Identity.scale(3.5, 3.5, 3.5).translate(0, 10, 20).rotateZ(Pi / 4).rotateX(Pi / 6),
      material = Material(color = Color(1, 0.3, 0.3)))

    val c2 = Cylinder(
      transform = Identity.scale(3.5, 3.5, 3.5).translate(0, 10, 20).rotateZ(-Pi / 4).rotateX(Pi / 6),
      material = Material(color = Color(0.3, 0.3, 1)))

    val c3 = Cylinder(
      minimum = -5,
      maximum = 5,
      closed = true,
      transform = Identity.scale(10, 3, 10).translate(-70, -30, 40).rotateX(-Pi / 4),
      material = Material(color = Color(0.3, 0.8, 0.3)))

    val c4 = Cylinder(
      minimum = -5,
      maximum = 5,
      transform = Identity.scale(10, 3, 10).translate(70, -30, 40).rotateX(-Pi / 4),
      material = Material(color = Color(0.3, 1, 0.3)))

    val light = PointLight(Point(10, 10, -10), Color(1, 1, 1))

    val world = World(light, floor :: c1 :: c2 :: c3 :: c4 :: Nil)
    val camera = Camera(500, 250, Pi / 1.8, Matrix4x4.viewTransform(Point(0, 3, -60), Point(0, 1, 0), Vector(0, 1, 0)))
    val canvas = camera.renderConcurrently(world)

    //    canvas.dumpPpmTo("cylinders-500x250.ppm")
    assert(canvas.toPpm == Sources.readString("/it/cylinders-500x250.ppm"))
  }

}
