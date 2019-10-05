package it.ray.tracer

import org.scalatest.FunSuite
import ray.tracer.{Canvas, Color, Matrix4x4, Point}
import testutil.Sources

import scala.math.Pi

class ClockIntegrationTest extends FunSuite {

  test("generate clock") {
    val width = 500
    val height = 500
    val canvas = Canvas(width, height)

    val radius = width * 3 / 8

    val white = Color(1, 1, 1)
    val rotY = Matrix4x4.Identity.rotateY(Pi / 6)

    var next = Point(0, 0, 1)

    for (_ <- 0 until 12) {
      val c = Point(next.x * radius + 250, next.y, next.z * radius + 250)
      canvas(c.x.toInt, c.z.toInt) = white
      next = rotY * next
    }

    //    canvas.dumpPpmTo("clock-500x500.ppm")
    assert(canvas.toPpm == Sources.readString("/it/clock-500x500.ppm"))
  }
}