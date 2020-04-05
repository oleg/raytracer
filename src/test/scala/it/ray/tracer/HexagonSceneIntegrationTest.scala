package it.ray.tracer
import ray.tracer.shape.ShapeFactory._
import org.scalatest.FunSuite
import ray.tracer.Matrix4x4.Identity
import ray.tracer._
import ray.tracer.shape.{Group, Shape}
import testutil.Sources

import scala.math.Pi

class HexagonSceneIntegrationTest extends FunSuite {

  test("generate hexagons") {

    val light = PointLight(Point(10, 10, -10), Color(1, 1, 1))

    val world = World(light, hexagon() :: Nil)
    val camera = Camera(500, 250, Pi / 3, Matrix4x4.viewTransform(Point(0, 1, -3), Point(0, 0, 0), Vector(0, 1, 0)))
    val canvas = camera.renderConcurrently(world)

    //    canvas.dumpPpmTo("hexagons-500x250.ppm")
    assert(canvas.toPpm == Sources.readString("/it/hexagons-500x250.ppm"))
  }

  private def hexagonCorner(): Shape =
    Sphere(transform = Identity
      .scale(0.25, 0.25, 0.25)
      .translate(0, 0, -1))

  private def hexagonEdge(): Shape =
    Cylinder(
      minimum = 0,
      maximum = 1,
      transform = Identity
        .scale(0.25, 1, 0.25)
        .rotateZ(-Pi / 2)
        .rotateY(-Pi / 6)
        .translate(0, 0, -1))

  private def hexagonSide(n: Int): Shape = {
    val side = Group(transform = Identity.rotateY(n * Pi / 3))
    side.add(hexagonCorner())
    side.add(hexagonEdge())
    side
  }

  private def hexagon(): Shape = {
    val hex = Group()
    for (n <- 0 to 5) {
      hex.add(hexagonSide(n))
    }
    hex
  }

}
