package ray.sample

import java.io.PrintWriter

import ray.tracer.Matrix4x4.Identity
import ray.tracer._

import scala.math.Pi

object CylinderScene {

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
      transform = Identity.translate(0, -20, 0)
    )

    val c1 = Cylinder(
      Identity.scale(3.5, 3.5, 3.5).translate(0, 10, 20).rotateZ(Pi / 4).rotateX(Pi / 6),
      Material(color = Color(1, 0, 0)))

    val c2 = Cylinder(
      Identity.scale(3.5, 3.5, 3.5).translate(0, 10, 20).rotateZ(-Pi / 4).rotateX(Pi / 6),
      Material(color = Color(0, 0, 1)))

    val light = PointLight(Point(10, 10, -10), Color(1, 1, 1))

    val world = World(light, floor :: c1 :: c2 :: Nil)
    val camera = Camera(1000, 500, Pi / 1.8, Matrix4x4.viewTransform(Point(0, 3, -40), Point(0, 1, 0), Vector(0, 1, 0)))
    val canvas = camera.renderConcurrently(world)

    new PrintWriter(s"cylinder-${System.currentTimeMillis()}.ppm") {
      write(canvas.toPpm)
      close()
    }
  }


}