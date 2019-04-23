package ray.sample

import java.io.PrintWriter

import ray._

object Ball {

  def main(args: Array[String]): Unit = {
    draw(args)
  }

  def draw(args: Array[String]): Unit = {

    val rayOrigin = Point(0, 0, -5)
    val wallZ = 10
    val wallSize = 7.0
    val canvasPixels = 100
    val width = canvasPixels
    val height = canvasPixels


    val pixelSize = wallSize / canvasPixels
    val half = wallSize / 2
    val canvas = Canvas(width, height)
    val white = Color(1, 1, 1)
    val sphere = Sphere()

    for (y <- 0 until canvasPixels) {
      val worldY = half - pixelSize * y
      for (x <- 0 until canvasPixels) {
        val worldX = -half + pixelSize * x
        val position = Point(worldX, worldY, wallZ)
        val r = Ray(rayOrigin, (position - rayOrigin).normalize)
        val xs = r.intersect(sphere)
        Intersections(xs: _*).hit.foreach(_ => canvas(x, y) = white)
      }
    }

    new PrintWriter("ball.ppm") {
      write(canvas.toPpm())
      close()
    }
  }


}
