package ray.sample

import java.io.PrintWriter

import ray.tracer.Matrix4x4.Identity
import ray.tracer._

import scala.math.Pi

object ConeScene {

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
      transform = Identity.translate(0, -5, 0)
    )

    val back = Plane(
      material = Material(
        specular = 0,
        refractiveIndex = 0,
        color = Color(0.3, 0.3, 0.3)),
      transform = Identity.rotateX(-Pi / 2).translate(0, 0, 100))

    val c1 = Cone(
      minimum = -1,
      maximum = 0,
      transform = Identity.rotateX(-Pi / 6).rotateZ(Pi / 6).translate(-3, 1, 0).scale(4, 4, 4),
      material = Material(color = Color(1, 0.3, 0.3)))

    val c2 = Cone(
      minimum = -1,
      maximum = 0,
      transform = Identity /*.rotateY(Pi / 6)*/ .translate(0, 1, 0).scale(4, 4, 4),
      material = Material(color = Color(1, 1, 0.3)))

    val cy2 = Cylinder(
      minimum = -10,
      maximum = 0,
      transform = Identity /*.rotateY(Pi / 6)*/ .translate(0, 0, 0).scale(4, 4, 4),
      material = Material(color = Color(1, 1, 0.3)))

    val c3 = Cone(
      minimum = -1,
      maximum = 0,
      transform = Identity.rotateX(-Pi / 6).rotateZ(-Pi / 6).translate(3, 1, 0).scale(4, 4, 4),
      material = Material(color = Color(0.3, 1, 1)))

    val light = PointLight(new Point(10, 10, -25), Color(1, 1, 1))

    val world = World(light, floor :: c1 :: c2 :: cy2 :: c3 :: back :: Nil)
    val camera = Camera(1000, 500, Pi / 1.8, Matrix4x4.viewTransform(new Point(0, 3, -15), new Point(0, 1, 0), Vector(0, 1, 0)))
    val canvas = camera.renderConcurrently(world)

    new PrintWriter(s"cone-${System.currentTimeMillis()}.ppm") {
      write(canvas.toPpm)
      close()
    }
  }


}