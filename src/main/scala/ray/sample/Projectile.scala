package ray.sample

import java.io.PrintWriter

import ray.tracer._

object Projectile {

  class Environment(val gravity: Vector, val wind: Vector)

  class Projectile(val position: Point, val velocity: Vector) {

    def tick(env: Environment): Projectile =
      new Projectile(
        position + velocity,
        velocity + env.gravity + env.wind)

  }

  def main(args: Array[String]): Unit = {
    draw(args)
  }

  def draw(args: Array[String]): Unit = {
    val canvas = Canvas(900, 500)
    val red = Color(1, 1, 1)
    val environment = new Environment(new Vector(0, -0.15, 0), new Vector(-0.02, 0, 0))
    var projectile = new Projectile(Point(0, 1, 0), new Vector(1, 1.8, 0).normalize * 11.25)
    do {
      canvas(projectile.position.x.toInt, canvas.height - projectile.position.y.toInt - 1) = red
      projectile = projectile.tick(environment)
    } while (projectile.position.y > 0 && projectile.position.y < canvas.height)

    new PrintWriter("projectile.ppm") {
      write(canvas.toPpm)
      close()
    }
  }

  def run(args: Array[String]): Unit = {
    val environment = new Environment(new Vector(0, -0.1, 0), new Vector(-0.01, 0, 0))
    var projectile = new Projectile(Point(0, 1, 0), new Vector(1, 1, 0).normalize)
    do {
      projectile = projectile.tick(environment)

      println(projectile.position)
      Thread.sleep(500)
    } while (projectile.position.y > 0)
  }

}



