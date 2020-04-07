package it.ray.tracer
import ray.tracer.shape.ShapeFactory._
import org.scalatest.funsuite.AnyFunSuite
import ray.tracer.Matrix4x4.Identity
import ray.tracer._
import testutil.Sources

import scala.math.Pi

class TriangleSceneIntegrationTest extends AnyFunSuite {

  test("generate triangles") {

    val floor = Plane(
      material = Material(
        reflective = 0.7,
        transparency = 0.2,
        refractiveIndex = 1.3,
        pattern = GradientPattern(Color(0.2, 0.2, 0.2), Color(0.8, 0.8, 0.8)))
    )

    val back = Plane(
      material = Material(
        reflective = 0.3,
        transparency = 0.1,
        refractiveIndex = 2,
        pattern = GradientPattern(Color(0.2, 0.2, 0.2), Color(0.8, 0.8, 0.8))),
      transform = Identity.rotateX(-Pi / 2).translate(0, 0, 4)
    )

    val t0 = Triangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0),
      transform = Identity.translate(-2, 0, 0.3))

    val t1 = Triangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0),
      transform = Identity.translate(1, 0, -0.4).scale(1.5, 1.5, 1.5))

    val t2 = Triangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0),
      transform = Identity.translate(1, 0, 0).rotateZ(Pi / 2.5).translate(-0.5, 0, 0))

    val light = PointLight(Point(10, 10, -10), Color(1, 1, 1))

    val world = World(light, floor :: t0 :: t1 :: t2 :: back :: Nil)
    val camera = Camera(500, 250, Pi / 3, Matrix4x4.viewTransform(Point(0, 3, -6), Point(0, 1, 0), Vector(0, 1, 0)))
    val canvas = camera.renderConcurrently(world)

    //canvas.dumpPpmTo("triangles-500x250.ppm")
    assert(canvas.toPpm == Sources.readString("/it/triangles-500x250.ppm"))
  }

}
