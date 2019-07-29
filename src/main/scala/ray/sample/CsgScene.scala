package ray.sample

import java.io.PrintWriter

import ray.tracer.Matrix4x4.Identity
import ray.tracer._

import scala.math.Pi

object CsgScene {

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
        refractiveIndex = 1.3),
      transform = Identity.translate(0, -10, 0)
    )

    val c1 = Cube(
      transform = Identity.scale(3.5, 3.5, 3.5),
      material = Material(color = Color(1, 0.3, 0.3)))

    val s1 = Sphere(
      transform = Identity.scale(4, 4, 4),
      material = Material(color = Color(0.3, 0.3, 1)))

    val csg1 = Csg(Operation.difference, c1, s1,
      transform = Identity.rotateX(Pi/6).rotateZ(Pi/6).rotateY(Pi/6).translate(-7, 0, 0))

    val c2 = Cube(
      transform = Identity.scale(4, 4, 4),
      material = Material(color = Color(1, 0.3, 0.3)))

    val s2 = Sphere(
      transform = Identity.scale(5, 5, 5),
      material = Material(color = Color(0.3, 0.3, 1)))

    val csg2 = Csg(Operation.difference, s2, c2,
      transform = Identity.rotateX(Pi/6).rotateZ(Pi/6).rotateY(Pi/6).translate(7, 0, 0))

    val light = PointLight(new Point(4, 18, -10), Color(1, 1, 1))

    val world = World(light, floor :: csg1 :: csg2 :: Nil)
    val camera = Camera(1000, 500, Pi / 1.8, Matrix4x4.viewTransform(new Point(0, 4, -19), new Point(0, 0, 0), Vector(0, 1, 0)))
    val canvas = camera.renderConcurrently(world)

    new PrintWriter(s"csg-${System.currentTimeMillis()}.ppm") {
      write(canvas.toPpm)
      close()
    }
  }


}