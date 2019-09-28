package samples

import java.io.PrintWriter

import ray.tracer.Matrix4x4.Identity
import ray.tracer._

import scala.math.Pi
import ray.shapes._

object Refraction {

  def main(args: Array[String]): Unit = {
    val l = System.currentTimeMillis() //todo remove
    draw(args)
    println(System.currentTimeMillis() - l)
  }

  def draw(args: Array[String]): Unit = {

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
    val camera = Camera(1000, 500, Pi / 3, Matrix4x4.viewTransform(Point(0, 3, -6), Point(0, 1, 0), Vector(0, 1, 0)))
    val canvas = camera.renderConcurrently(world)

    new PrintWriter(s"refraction-${System.currentTimeMillis()}.ppm") {
      write(canvas.toPpm)
      close()
    }
  }


}