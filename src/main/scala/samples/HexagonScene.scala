package samples

import java.io.PrintWriter

import ray.shapes._
import ray.tracer.Matrix4x4.Identity
import ray.tracer._

import scala.math.Pi

object HexagonScene {

  def main(args: Array[String]): Unit = {
    val l = System.currentTimeMillis() //todo remove
    draw(args)
    println(System.currentTimeMillis() - l)
  }

  def draw(args: Array[String]): Unit = {


    val light = PointLight(Point(10, 10, -10), Color(1, 1, 1))

    val world = World(light, hexagon() :: Nil)
    val camera = Camera(1000, 500, Pi / 3, Matrix4x4.viewTransform(Point(0, 1, -3), Point(0, 0, 0), Vector(0, 1, 0)))
    val canvas = camera.renderConcurrently(world)

    new PrintWriter(s"hexagon-${System.currentTimeMillis()}.ppm") {
      write(canvas.toPpm)
      close()
    }
  }

  private def hexagon(): Shape = {
    val hex = Group()
    for (n <- 0 to 5) {
      hexagonSide(n, hex)
    }
    hex
  }

  private def hexagonSide(n: Int, parent: Shape): Shape = {
    val side = Group(transform = Identity.rotateY(n * Pi / 3), parent = parent)
//    side.add(hexagonCorner(side))
//    side.add(hexagonEdge(side))
    hexagonCorner(side)
    hexagonEdge(side)
    side
  }

  private def hexagonCorner(shape: Shape): Shape =
    Sphere(transform = Identity
      .scale(0.25, 0.25, 0.25)
      .translate(0, 0, -1))

  private def hexagonEdge(shape: Shape): Shape =
    Cylinder(
      minimum = 0,
      maximum = 1,
      transform = Identity
        .scale(0.25, 1, 0.25)
        .rotateZ(-Pi / 2)
        .rotateY(-Pi / 6)
        .translate(0, 0, -1))

}