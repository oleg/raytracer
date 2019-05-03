package ray.sample

import java.io.PrintWriter

import ray.tracer.Matrix4x4.Identity
import ray.tracer._

import scala.math.Pi

object PlaneComposition {

  def main(args: Array[String]): Unit = {
    draw(args)
  }

  def draw(args: Array[String]): Unit = {

    val floor = Plane(
      material = Material(
        pattern = StripePattern(Color(0.5, 0.3, 0), Color(0, 0.5, 1),
          transform = Identity.rotateX(Pi / 2).rotateY(Pi / 2).rotateZ(Pi / 2))))
    val back = Plane(
      Identity.rotateX(Pi / 2).translate(0, 0, 4)
    )

    val left = Sphere(
      Identity.scale(1, 0.33, 0.33).translate(-1.5, 0.33, -0.75),
      Material(color = Color(1, 0.1, 0.1), diffuse = 0.7, specular = 0.3))

    val middle = Sphere(
      Identity.translate(-0.5, 1, 0.2),
      Material(
        color = Color(0.1, 0.1, 1),
        diffuse = 0.7,
        specular = 0.3,
        pattern = StripePattern(Color(1, 0, 0), Color(0, 1, 0),
          transform = Matrix4x4.Scaling(0.5, 0.5, 0.5))))

    val right = Sphere(
      Identity.scale(0.5, 0.8, 0.5).translate(1.5, 0.5, -0.5),
      Material(color = Color(0.1, 1, 0.1), diffuse = 0.7, specular = 0.3))

    val light = PointLight(Point(10, 10, -10), Color(1, 1, 1))

    val world = World(light, back :: floor :: middle :: right :: left :: Nil)
    val camera = Camera(1000, 500, Pi / 3, Matrix4x4.viewTransform(Point(0, 3, -6), Point(0, 1, 0), Vector(0, 1, 0)))
    val canvas = camera.render(world)

    new PrintWriter("plane.ppm") {
      write(canvas.toPpm)
      close()
    }
  }


}