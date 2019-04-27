package ray.sample

import java.io.PrintWriter

import ray._

import scala.math.Pi

object Clock {


  def main(args: Array[String]): Unit = {
    draw(args)
  }

  def draw(args: Array[String]): Unit = {
    val width = 500
    val height = 500
    val canvas = Canvas(width, height)

    val radius = width * 3 / 8

    val white = Color(1, 1, 1)
    val rotY = Matrix4x4.Identity.rotateY(Pi / 6)

    var next = Point(0, 0, 1)

    for (i <- 0 until 12) {
      val c = Point(next.x * radius + 250, next.y, next.z * radius + 250)
      canvas(c.x.toInt, c.z.toInt) = white
      next = rotY * next
    }

    new PrintWriter("clock.ppm") {
      write(canvas.toPpm)
      close()
    }
  }


}



