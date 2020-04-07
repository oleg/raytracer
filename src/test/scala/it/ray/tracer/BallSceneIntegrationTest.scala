package it.ray.tracer
import ray.tracer.shape.ShapeFactory._
import org.scalatest.funsuite.AnyFunSuite
import ray.tracer.Matrix4x4.Identity
import ray.tracer._
import testutil.Sources

import scala.math.Pi

class BallSceneIntegrationTest extends AnyFunSuite {
  test("generate ball scene") {

    val floor = Sphere(
      Identity.scale(10, 0.01, 10),
      Material(color = Color(1, 0.9, 0.9), specular = 0))

    val leftWall = Sphere(
      Identity
        .scale(10, 0.01, 10)
        .rotateX(Pi / 2)
        .rotateY(-Pi / 4)
        .translate(0, 0, 5),
      Material(color = Color(1, 0.9, 0.9), specular = 0))

    val rightWall = Sphere(
      Identity
        .scale(10, 0.01, 10)
        .rotateX(Pi / 2)
        .rotateY(Pi / 4)
        .translate(0, 0, 5),
      Material(color = Color(1, 0.9, 0.9), specular = 0))

    val middle = Sphere(
      Identity.translate(-0.5, 1, 0.5),
      Material(color = Color(0.1, 1, 0.5), diffuse = 0.7, specular = 0.3))

    val right = Sphere(
      Identity.scale(0.5, 0.5, 0.5).translate(1.5, 0.5, -0.5),
      Material(color = Color(0.5, 1, 0.1), diffuse = 0.7, specular = 0.3))

    val left = Sphere(
      Identity.scale(0.33, 0.33, 0.33).translate(-1.5, 0.33, -0.75),
      Material(color = Color(1, 0.8, 0.1), diffuse = 0.7, specular = 0.3))

    val light = PointLight(Point(-10, 10, -10), Color(1, 1, 1))

    val world = World(light, floor :: leftWall :: rightWall :: middle :: right :: left :: Nil)
    val camera = Camera(500, 250, Pi / 3, Matrix4x4.viewTransform(Point(0, 1.5, -5), Point(0, 1, 0), Vector(0, 1, 0)))
    val canvas = camera.renderConcurrently(world)

    //    canvas.dumpPpmTo("ball-scene-500x250.ppm")
    assert(canvas.toPpm == Sources.readString("/it/ball-scene-500x250.ppm"));
  }
}
