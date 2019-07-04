package ray.sample

import java.io.PrintWriter

import ray.tracer._

import scala.io.Source
import scala.math.Pi

object ObjFileScene {

  def main(args: Array[String]): Unit = {
    val l = System.currentTimeMillis() //todo remove
    draw(args)
    println(System.currentTimeMillis() - l)
  }

  def draw(args: Array[String]): Unit = {

    val obj = new ObjFileParser().parse(Source.fromResource("teapot-low.obj"))
    //todo fix
    val group1 = Group(
      transform = Matrix4x4.Identity
        .rotateX(-Pi / 2)
        .translate(0, -5, 0))

    group1.add(obj.mainGroup)

    val world = World(
      PointLight(Point(20, 20, 30), Color.white),
      group1 :: Nil
    )
    val f = 3
    val camera = Camera(100 * f, 50 * f, Pi / 2, Matrix4x4.viewTransform(Point(0, 10, 20), Point(0, 1, 0), Vector(0, 1, 0)))
    val canvas = camera.renderConcurrently(world)

    new PrintWriter(s"obj-${System.currentTimeMillis()}.ppm") {
      write(canvas.toPpm)
      close()
    }
  }

}