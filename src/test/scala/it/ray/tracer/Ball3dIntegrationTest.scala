package it.ray.tracer

import org.scalatest.FunSuite
import ray.tracer._
import ray.tracer.shape.ShapeFactory._
import testutil.Sources

class Ball3dIntegrationTest extends FunSuite {
  test("generate 3d ball") {
    val rayOrigin = Point(0, 0, -5)
    val wallZ = 10
    val wallSize = 7.0
    val canvasPixels = 500 //300 //100
    val width = canvasPixels
    val height = canvasPixels

    val pixelSize = wallSize / canvasPixels
    val half = wallSize / 2
    val canvas = Canvas(width, height)
    val white = Color(1, 1, 1)

    val transform = Matrix4x4.Identity //Matrix4x4.Shearing(1, 0, 0, 0, 0, 0) * Matrix4x4.Scaling(0.5, 1, 1)
    val sphere = Sphere(transform, Material(color = Color(0.2, 0.8, 0.3)))

    val lightPosition = Point(-10, 10, -10)
    val lightColor = Color(1, 1, 1)
    val light = PointLight(lightPosition, lightColor)

    for (y <- 0 until canvasPixels) {
      val worldY = half - pixelSize * y
      for (x <- 0 until canvasPixels) {

        val worldX = -half + pixelSize * x
        val position = Point(worldX, worldY, wallZ)

        val ray = Ray(rayOrigin, (position - rayOrigin).normalize)

        sphere.intersect(ray)
          .hit
          .foreach({ h =>
            val point = ray.position(h.t)
//            val normal = h.obj.normalAt(point, null)
            val i = Intersection(h.t, h.obj, h.obj.transform :: Nil)
            val normal = i.myNormalAt(h.obj, point)
            val eye = -ray.direction

            canvas(x, y) = h.obj.material.lighting(light, h.obj, point, eye, normal, false)
          })
      }
    }

    assert(canvas.toPpm == Sources.readString("/it/ball-3d-500x500.ppm"))
  }
}
