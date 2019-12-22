package it.ray.tracer
import ray.tracer.Shape.Sphere
import org.scalatest.FunSuite
import ray.tracer.Matrix4x4.Identity
import ray.tracer._
import testutil.Sources

import scala.math.Pi

class RefractionIntegrationTest extends FunSuite {

  test("generate refractions") {

    val floor = Plane(
      material = Material(
        reflective = 0.7,
        transparency = 0.2,
        refractiveIndex = 1.3,
        pattern = CheckersPattern(Color.black, Color.white))
    )

    val back = Plane(
      material = Material(
        reflective = 0.3,
        transparency = 0.1,
        refractiveIndex = 2,
        pattern = CheckersPattern(Color.black, Color.white)),
      transform = Identity.rotateX(-Pi / 2).translate(0, 0, 4)
    )

    val b0 = Sphere(
      Identity.translate(-2.4, 1, 0.2),
      Material(
        //        specular = 1,
        transparency = 0.3,
        reflective = 0.3,
        refractiveIndex = 1,
        ambient = 0.2,
        color = Color.white
      ))

    val b1 = Sphere(
      Identity.translate(-0.1, 1, 0.2),
      Material(
        transparency = 0.5,
        reflective = 0.3,
        refractiveIndex = 1.2,
        color = Color(0, 0, 0.4)
      ))

    val b3 = Sphere(
      Identity.translate(2.2, 1, 0.2),
      Material(
        transparency = 0.7,
        reflective = 0.3,
        refractiveIndex = 1.5,
        color = Color(0.4, 0, 0)
      ))

    val light = PointLight(Point(10, 10, -10), Color(1, 1, 1))

    val world = World(light, floor :: b0 :: b1 :: b3 :: back :: Nil)
    val camera = Camera(500, 250, Pi / 3, Matrix4x4.viewTransform(Point(0, 3, -6), Point(0, 1, 0), Vector(0, 1, 0)))
    val canvas = camera.renderConcurrently(world)

    //    canvas.dumpPpmTo("refractions-500x250.ppm")
    assert(canvas.toPpm == Sources.readString("/it/refractions-500x250.ppm"))
  }

}
