package ray.sample

import java.io.PrintWriter

import ray.tracer.Matrix4x4.Identity
import ray.tracer._

import scala.math.Pi

object BallComposition {

  def main(args: Array[String]): Unit = {
    draw(args)
  }

  def draw(args: Array[String]): Unit = {

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
    val camera = Camera(1000, 500, Pi / 3, Matrix4x4.viewTransform(Point(0, 1.5, -5), Point(0, 1, 0), Vector(0, 1, 0)))
    val canvas = camera.renderConcurrently(world)

    new PrintWriter("threeballs.ppm") {
      write(canvas.toPpm)
      close()
    }
  }


}