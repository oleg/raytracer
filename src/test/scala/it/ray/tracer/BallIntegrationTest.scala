package it.ray.tracer
import ray.tracer.ShapeFactory._
import org.scalatest.FunSuite
import ray.tracer._
import testutil.Sources

class BallIntegrationTest extends FunSuite {

  test("generate ball") {
    val rayOrigin = Point(0, 0, -5)
    val wallSize = 7.0
    val canvasPixels = 100
    val pixelSize = wallSize / canvasPixels
    val half = wallSize / 2
    val canvas = Canvas(canvasPixels, canvasPixels)
    val white = Color(1, 1, 1)
    val transform = Matrix4x4.Shearing(1, 0, 0, 0, 0, 0) * Matrix4x4.Scaling(0.5, 1, 1)
    val sphere = Sphere(transform)

    for (y <- 0 until canvasPixels) {
      val worldY = half - pixelSize * y
      for (x <- 0 until canvasPixels) {
        val worldX = -half + pixelSize * x
        val position = Point(worldX, worldY, 10)
        val ray = Ray(rayOrigin, (position - rayOrigin).normalize)
        sphere.intersect(ray).hit.foreach(_ => canvas(x, y) = white)
      }
    }

    assert(canvas.toPpm == Sources.readString("/it/ball-100x100.ppm"))
  }
}