package samples
import ray.shapes._
import java.io.PrintWriter

import ray.tracer.Matrix4x4.Identity
import ray.tracer._

import scala.math.Pi

object Cubes {

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
        pattern = GradientPattern(Color.black, Color.white,
          transform = Identity.scale(7, 1, 1).rotateY(math.Pi / 2).translate(0, 0, -10)))
    )

    val back = Plane(
      material = Material(
        reflective = 0.3,
        transparency = 0.1,
        refractiveIndex = 2,
        pattern = RingPattern(Color(0.2, 0.2, 0.2), Color(0.8, 0.8, 0.8))),
      transform = Identity.rotateX(-Pi / 2).translate(0, 0, 4)
    )

    val b0 = Cube(
      Identity.translate(2.2, 1, -1.5)
        .scale(0.4, 0.5, 0.6),
      Material(
        //        specular = 1,
        transparency = 0.3,
        reflective = 0.3,
        refractiveIndex = 1,
        ambient = 0.2,
        color = Color(0.9, 0.8, 0.2)
      ))

    val b1 = Cube(
      Identity.translate(-0.1, 1, 0)
        .scale(0.9, 0.8, 0.7),
      Material(
        transparency = 0.5,
        reflective = 0.3,
        refractiveIndex = 1.2,
        color = Color(0.9, 0.2, 0.2)
      ))

    val b3 = Cube(
      Identity.translate(-1.5, 1, 1.3)
        .scale(1, 1, 1),
      Material(
        transparency = 0.7,
        reflective = 0.3,
        refractiveIndex = 1.5,
        color = Color(0.1, 0.1, 0.1)
      ))

    val light = PointLight(Point(10, 10, -10), Color(1, 1, 1))

    val world = World(light, floor :: b0 :: b1 :: b3 :: back :: Nil)
    val f = 20
    val camera = Camera(100 * f, 50 * f, Pi / 3, Matrix4x4.viewTransform(Point(0, 3, -6), Point(0, 1, 0), Vector(0, 1, 0)))
    val canvas = camera.renderConcurrently(world)

    new PrintWriter(s"cubes-${System.currentTimeMillis()}.ppm") {
      write(canvas.toPpm)
      close()
    }
  }


}